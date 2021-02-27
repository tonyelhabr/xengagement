
#' @noRd
.str_replace_text <- function(x, i) {
  x %>% stringr::str_replace('(^.*)\\s\\(([0-9.]+)\\)\\s([0-9]+)[-]([0-9]+)\\s\\(([0-9.]+)\\)\\s(.*$)', sprintf('\\%d', i))
}

#' @noRd
.remove_emoticons <- function(x) {
  iconv(x, 'latin1', 'ASCII', sub='') %>% stringr::str_trim()
}

#' @noRd
.add_estimated_follower_count_col <-
  function(data,
           suffix = .get_valid_suffixes(),
           latest_date,
           train = TRUE,
           retrieve = !train) {
    .validate_suffix(suffix)
    col_created_at_sym <- sprintf('created_at_%s', suffix) %>% sym()
    col_diff <- sprintf('date_diff_%s', suffix)
    col_diff_sym <- col_diff %>% sym()
    col_diff_latest_sym <- sprintf('%s_latest', col_diff) %>% sym()
    col_followers_count_sym <-
      sprintf('followers_count_%s', suffix) %>% sym()
    col_res_sym <-
      sprintf('estimated_followers_count_%s', suffix) %>% sym()
    
    if (!train & retrieve) {
      
      teams_distinct <- data %>% .distinct12_at(col = 'team', suffix = .get_valid_suffixes())
      users <-
        team_accounts_mapping %>% 
        dplyr::semi_join(teams_distinct, by = 'team') %>% 
        dplyr::pull(user_id)
      if(length(users) == 0L) {
        .display_warning('Could not retrieve most up-to-date follower count for {length(teams_distinct)} teams. Using pre-saved info.')
      } else {
        team_accounts <-
          users %>% 
          rtweet::lookup_users() %>% 
          dplyr::select(user_id, followers_count)
        team_accounts_mapping <-
          team_accounts_mapping %>% 
          dplyr::select(-followers_count) %>% 
          dplyr::inner_join(team_accounts, by = 'user_id')
      }
    } else {
      team_accounts_mapping <- team_accounts_mapping %>% dplyr::select(-user_id)
    }
    
    team_col <- sprintf('team_%s', suffix)
    # suppressMessages(
    res <-
      data %>%
      dplyr::inner_join(
        team_accounts_mapping %>% dplyr::rename_all( ~ sprintf('%s_%s', .x, suffix)),
        by = team_col
      ) %>%
      dplyr::mutate(
        !!col_diff_sym := !!latest_date - lubridate::date(!!col_created_at_sym),
        !!col_diff_latest_sym := !!latest_date - created_date,
        dplyr::across(dplyr::matches(col_diff), as.numeric),
        !!col_res_sym := ((!!col_diff_sym-!!col_diff_latest_sym) / !!col_diff_sym) ^1 * !!col_followers_count_sym
      ) %>%
      dplyr::select(
        -dplyr::matches(col_diff),
        -!!col_created_at_sym,
        -!!col_followers_count_sym
      )
    # )
    res
  }

#' @noRd
.add_estimated_follower_count_cols <- function(data, ...) {
  # browser()
  data %>% 
    .add_estimated_follower_count_col('h', ...) %>% 
    .add_estimated_follower_count_col('a', ...)
}

#' @noRd
.fix_team_col <- function(data, suffix = .get_valid_suffixes()) {
  # browser()
  .validate_suffix(suffix)
  col_team_sym <- sprintf('team_%s', suffix) %>% sym()
  col_team_correct_sym <- sprintf('team_correct_%s', suffix) %>% sym()
  # team_corrections <- .get_team_corrections()
  team_col <- sprintf('team_%s', suffix)
  data %>% 
    dplyr::left_join(
      team_corrections %>% dplyr::rename_all(~sprintf('%s_%s', .x, suffix)),
      by = team_col
    ) %>% 
    dplyr::mutate(
      dplyr::across(!!col_team_sym, ~dplyr::coalesce(!!col_team_correct_sym, .x))
    ) %>% 
    dplyr::select(-!!col_team_correct_sym)
}

#' @noRd
.fix_team_cols <- function(data) {
  data %>% 
    .fix_team_col('h') %>% 
    .fix_team_col('a')
}

#' @noRd
.retrieve_matches_538 <- # memoise::memoise({
  function() {
  matches <- 
    readr::read_csv(
      'https://projects.fivethirtyeight.com/soccer-api/club/spi_matches.csv',
      col_types = readr::cols(
        .default = readr::col_double(),
        date = readr::col_date(format = ''),
        league = readr::col_character(),
        team1 = readr::col_character(),
        team2 = readr::col_character()
      )
    ) %>% 
    dplyr::filter(league == 'Barclays Premier League' & season >= 2019) %>% 
    dplyr::select(-c(league, league_id)) %>% 
    dplyr::rename(date_538 = date, team_538_h = team1, team_538_a = team2, probtie_538 = probtie) %>% 
    dplyr::rename_with(~stringr::str_replace(.x, '1$', '_538_h'), dplyr::matches('1$')) %>% 
    dplyr::rename_with(~stringr::str_replace(.x, '2$', '_538_a'), dplyr::matches('2$'))
  matches
}
# })

#' @noRd
.add_cols_538 <- function(data, matches = .retrieve_matches_538()) {
  # matches = .retrieve_matches_538()
  data %>% 
    dplyr::left_join(matches, by = c('season', 'team_538_h', 'team_538_a'))
}


#' Transform tweets
#' 
#' Transform tweets into format for modeling. This can be saved and combined with SHAP values later. Follower growth of the xGPhilophy account is assumed to be linear per tweet, while growth of the team accounts is assumed to follow a concave curve.
#' @param tweets Tweets retrieved with `retrieve_tweets`
#' @param ... Not currently used
#' @param train If `TRUE`, then updates team follower account numbers. Otherwise, uses an extrapolated based on the last retrieved numbers.
#' @param first_followers_count Assumed number of xGPhilospher followers around the end of 2019. We have to make an assumption so that we can do interpolation of growth of followers.
#' @export
transform_tweets <- function(tweets, ..., train = TRUE, first_followers_count = 5000) {
  
  now <- lubridate::now()
  n_hour_fresh <- .get_n_hour_fresh()
  res_init <-
    tweets %>%
    dplyr::select(
      status_id,
      created_at,
      retweet_count,
      favorite_count,
      text
    ) %>% 
    dplyr::mutate(is_fresh = dplyr::if_else(created_at <= (!!now - lubridate::hours(n_hour_fresh)), FALSE, TRUE))
  
  latest_tweet <- tweets %>% dplyr::slice_max(created_at, with_ties = FALSE)
  latest_followers_count <- latest_tweet$followers_count
  latest_date <- latest_tweet$created_at %>% lubridate::date()
  
  followers_count_diff <- latest_followers_count - first_followers_count
  res_init <-
    res_init %>% 
    # This is a linear estimate of follower count at the tweet time.
    dplyr::mutate(
      idx = dplyr::row_number(created_at),
      estimated_followers_count = !!first_followers_count + round((idx / max(idx)) * !!followers_count_diff, 0)
    ) %>% 
    dplyr::select(-idx)
  
  suppressWarnings(
    res_proc <-
      res_init %>% 
      # Drop half time scores, and just anything with commas or new lines since those aren't score line tweets.
      dplyr::filter(text %>% stringr::str_detect('^HT|\\,|\\n', negate = TRUE)) %>%
      # We know that a score line tweet has this.
      dplyr::filter(text %>% stringr::str_detect('\\(')) %>%
      dplyr::mutate(
        dplyr::across(c(favorite_count, retweet_count), list(log = ~log(.x + 1))),
        dplyr::across(
          created_at,
          list(
            created_date = lubridate::date
          ),
          .names = '{fn}'
        ),
        dplyr::across(
          text, 
          # text with 'FT: ' at the beginning
          ~stringr::str_remove(.x, '^FT[:]\\s+') %>% 
            # text that ends with a twitter url... this isn't completely robust, but it should be fine
            stringr::str_remove('\\s+https?[:][\\/][\\/]t[.]co.*$')
        ),
        # Warnings here.
        dplyr::across(
          text,
          list(
            team_h = ~ .str_replace_text(.x, 1) %>% .remove_emoticons(),
            xg_h = ~ .str_replace_text(.x, 2) %>% as.numeric(),
            g_h = ~ .str_replace_text(.x, 3) %>% as.integer(),
            g_a = ~ .str_replace_text(.x, 4) %>% as.integer(),
            xg_a = ~ .str_replace_text(.x, 5) %>% as.numeric(),
            team_a = ~ .str_replace_text(.x, 6) %>% .remove_emoticons()
          ),
          .names = '{fn}'
        )
      ) %>%
      # select(-text) %>% 
      # Drop non-score line tweets that weren't caught by previous filter.
      tidyr::drop_na(xg_h, g_h, g_a, xg_a) %>%
      dplyr::mutate(
        season = dplyr::if_else(created_date >= lubridate::ymd('20200912'), 2020L, 2019L)
      ) %>% 
      # There's a Biden Trump tweet that won't get past this filter. This filter helps overcome other weird tweets.
      dplyr::filter(g_h <= 10 & g_a <= 10 & xg_h <= 10 & xg_a <= 10) %>% 
      .fix_team_cols() %>% 
      # Update since non-EPL teams are now being tweeted on another account... Use inner_join instead of left_join
      .add_estimated_follower_count_cols(latest_date = latest_date, train = train) %>%  # , ...) %>% 
      .add_cols_538() %>% 
      dplyr::mutate(
        # is_gt_h = dplyr::if_else(xg_h - g_h > 0, 1L, 0L),
        # is_gt_a = dplyr::if_else(xg_a - g_a > 0, 1L, 0L),
        xgd_h2a = xg_h - xg_a,
        gd_h2a = g_h - g_a,
        # d_agree_h2a = 
        #   dplyr::case_when(
        #     xgd_h2a > 0 & gd_h2a > 0 ~ 1L,
        #     xgd_h2a < 0 & gd_h2a < 0 ~ 1L,
        #     TRUE ~ 0L
        #   ),
        d_h2a = xgd_h2a - gd_h2a
      ) %>% 
      # Don't keep games where neither side's followers can be estimated.
      # dplyr::filter(!is.na(estimated_followers_count_a) & !is.na(estimated_followers_count_h)) %>% 
      dplyr::select(-created_date) %>% 
      dplyr::arrange(created_at) %>% 
      dplyr::mutate(
        idx = dplyr::row_number(created_at)
      ) %>% 
      dplyr::relocate(idx, dplyr::matches('^team_'), dplyr::matches('^estimated_followers_count_'))
  )
  
  # standings <- .retrieve_understatr()
  # standings <- standings %>% dplyr::select(-c(lg, g)) %>% dplyr::rename(team_understat = team)
  # res
  # res %>% dplyr::inner_join(standings %>% dplyr::rename_all(~sprintf('%s_h', .x)))
  
  .f_distinct <- function(suffix = .get_valid_suffixes()) {
    res_proc %>% 
      dplyr::distinct(
        team = !!sym(sprintf('team_%s', suffix)), 
        created_at, 
        favorite_count, 
        retweet_count
      )
  }
  
  res_grps <- dplyr::bind_rows(.f_distinct('h'), .f_distinct('a'))
  
  res_lag <-
    res_grps %>% 
    dplyr::group_by(team) %>% 
    dplyr::mutate(
      dplyr::across(
        dplyr::matches('^(favorite|retweet)_count$'),
        list(
          lag1 = ~dplyr::lag(.x, 1L)
        )
      )
    ) %>% 
    dplyr::ungroup()
  res_lag
  
  .f_join_rename <- function(data, suffix = .get_valid_suffixes()) {
    data %>% 
      dplyr::left_join(
        res_lag %>% 
          dplyr::rename_with(
            ~sprintf('%s_%s', .x, suffix),
            -c(created_at)
          ),
        by = c('created_at', sprintf('team_%s', suffix))
      )
  }
  
  res <-
    res_proc %>% 
    .f_join_rename('h') %>% 
    .f_join_rename('a')

  if(train) {
    # Getting weird error without .data$idx
    res <-
      res %>% 
      dplyr::mutate(
        dplyr::across(
          dplyr::matches('^(favorite|retweet)_count$'),
          list(prnk = ~dplyr::percent_rank(.x))
        ),
        wt1 = dplyr::percent_rank(favorite_count_prnk + retweet_count_prnk)^2,
        wt2 = dplyr::percent_rank(.data$idx)^2,
        wt = dplyr::percent_rank(wt1 + wt2)^2
      ) %>% 
      dplyr::select(-c(wt1, wt2)) %>% 
      dplyr::relocate(idx, wt)
  }
  
  res
}

