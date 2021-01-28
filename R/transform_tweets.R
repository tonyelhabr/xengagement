
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

      tms_distinct <- data %>% .distinct12_at(col = 'tm', suffix = .get_valid_suffixes())
      users <-
        tm_accounts_mapping %>% 
        dplyr::semi_join(tms_distinct, by = 'tm') %>% 
        dplyr::pull(user_id)
      if(length(users) == 0L) {
        .display_warning('Could not retrieve most up-to-date follower count for {length(tms_distinct)} teams. Using pre-saved info.')
      } else {
        tm_accounts <-
          users %>% 
          rtweet::lookup_users() %>% 
          dplyr::select(user_id, followers_count)
        tm_accounts_mapping <-
          tm_accounts_mapping %>% 
          dplyr::select(-followers_count) %>% 
          dplyr::inner_join(tm_accounts, by = 'user_id')
      }
    } else {
      tm_accounts_mapping <- tm_accounts_mapping %>% dplyr::select(-user_id)
    }
    
    tm_col <- sprintf('tm_%s', suffix)
    # suppressMessages(
    res <-
      data %>%
      dplyr::left_join(
        tm_accounts_mapping %>% dplyr::rename_all( ~ sprintf('%s_%s', .x, suffix)),
        by = tm_col
      ) %>%
      dplyr::mutate(
        !!col_diff_sym := !!latest_date - lubridate::date(!!col_created_at_sym),
        !!col_diff_latest_sym := !!latest_date - created_date,
        dplyr::across(dplyr::matches(col_diff), as.numeric),
        !!col_res_sym := ((!!col_diff_sym-!!col_diff_latest_sym) / !!col_diff_sym) ^0.5 * !!col_followers_count_sym
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
.fix_tm_col <- function(data, suffix = .get_valid_suffixes()) {
  # browser()
  .validate_suffix(suffix)
  col_tm_sym <- sprintf('tm_%s', suffix) %>% sym()
  col_tm_correct_sym <- sprintf('tm_correct_%s', suffix) %>% sym()
  # tm_corrections <- .get_tm_corrections()
  tm_col <- sprintf('tm_%s', suffix)
  data %>% 
    dplyr::left_join(
      tm_corrections %>% dplyr::rename_all(~sprintf('%s_%s', .x, suffix)),
      by = tm_col
    ) %>% 
    dplyr::mutate(
      dplyr::across(!!col_tm_sym, ~dplyr::coalesce(!!col_tm_correct_sym, .x))
    ) %>% 
    dplyr::select(-!!col_tm_correct_sym)
}

#' @noRd
.fix_tm_cols <- function(data) {
  data %>% 
    .fix_tm_col('h') %>% 
    .fix_tm_col('a')
}

#' @noRd
.fourier_term <- function(x, period, f = sin, order) {
  f(2 * order * pi * x / period)
}

#' @noRd
.hour_fourier_term <- function(...) {
  .fourier_term(period = 24, ...)
}

#' @noRd
.wday_fourier_term <- function(...) {
  .fourier_term(period = 7, ...)
}

#' Transform tweets
#' 
#' Transform tweets into format for modeling. This can be saved and combined with SHAP values later. Follower growth of the xGPhilophy account is assumed to be linear per tweet, while growth of the team accounts is assumed to follow a concave curve.
#' @param tweets Tweets retrieved with `retrieve_tweets`
#' @param ... Not currently used
#' @param train If `TRUE`, then updates team follower account numbers. Otherwise, uses an extrapolated based on the last retrieved numbers.
#' @param first_followers_count Assumed number of xGPhilospher followers around the end of 2019. We have to make an assumption so that we can do interpolation of growth of followers.
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
  
  latest_tweet <- tweets %>% dplyr::slice_max(created_at)
  latest_followers_count <- latest_tweet$followers_count
  latest_date <- latest_tweet$created_at %>% lubridate::date()
  # if(train) {
  #   
  #   followers_count_diff <- latest_followers_count - first_followers_count
  #   res_init <-
  #     res_init %>% 
  #     # This is a linear estimate of follower count at the tweet time.
  #     dplyr::mutate(
  #       idx = dplyr::row_number(created_at),
  #       estimated_followers_count = !!first_followers_count + round((idx / max(idx)) * !!followers_count_diff, 0)
  #     ) %>% 
  #     dplyr::select(-idx)
  # } else {
  #   res_init <-
  #     res_init %>% 
  #     dplyr::mutate(estimated_followers_count = !!latest_followers_count)
  # }
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
    res <-
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
            hour = lubridate::hour,
            wday = ~lubridate::wday(.x) %>% as.integer(),
            created_date = lubridate::date
          ),
          .names = '{fn}'
        ),
        # is_weekend = if_else(wday %in% c(1L, 7L), TRUE, FALSE),
        dplyr::across(
          hour,
          list(
            x1 = ~.hour_fourier_term(.x, f = sin, order = 1),
            y1 = ~.hour_fourier_term(.x, f = cos, order = 1),
            x2 = ~.hour_fourier_term(.x, f = sin, order = 2),
            y2 = ~.hour_fourier_term(.x, f = sin, order = 2)
          )
        ),
        dplyr::across(
          wday,
          list(
            x1 = ~.wday_fourier_term(.x, f = sin, order = 1),
            y1 = ~.wday_fourier_term(.x, f = cos, order = 1),
            x2 = ~.wday_fourier_term(.x, f = sin, order = 2),
            y2 = ~.wday_fourier_term(.x, f = sin, order = 2)
          )
        ),
        dplyr::across(
          text, 
          # text with "FT: " at the beginning
          ~stringr::str_remove(.x, '^FT[:]\\s+') %>% 
            # text that ends with a twitter url... this isn't completely robust, but it should be fine
            stringr::str_remove('\\s+https?[:][\\/][\\/]t[.]co.*$')
        ),

        # Warnings here.
        dplyr::across(
          text,
          list(
            tm_h = ~ .str_replace_text(.x, 1) %>% .remove_emoticons(),
            xg_h = ~ .str_replace_text(.x, 2) %>% as.numeric(),
            g_h = ~ .str_replace_text(.x, 3) %>% as.integer(),
            g_a = ~ .str_replace_text(.x, 4) %>% as.integer(),
            xg_a = ~ .str_replace_text(.x, 5) %>% as.numeric(),
            tm_a = ~ .str_replace_text(.x, 6) %>% .remove_emoticons()
          ),
          .names = '{fn}'
        )
      ) %>%
      # select(-text) %>% 
      # Drop non-score line tweets that weren't caught by previous filter.
      tidyr::drop_na(xg_h, g_h, g_a, xg_a) %>%
      # There's a Biden Trump tweet that won't get past this filter. This filter helps overcome other weird tweets.
      dplyr::filter(g_h <= 10 & g_a <= 10 & xg_h <= 10 & xg_a <= 10)
  )

  res <-
    res %>% 
    .fix_tm_cols() %>% 
    .add_estimated_follower_count_cols(latest_date = latest_date, train = train) %>%  # , ...) %>% 
    # Don't keep games where neither side's followers can be estimated.
    # dplyr::filter(!is.na(estimated_followers_count_a) & !is.na(estimated_followers_count_h)) %>% 
    dplyr::select(-created_date) %>% 
    dplyr::arrange(created_at) %>% 
    dplyr::mutate(
      idx = dplyr::row_number(created_at)
    ) %>% 
    dplyr::relocate(idx, dplyr::matches('^tm_'), dplyr::matches('^estimated_followers_count_'))
  if(train) {
    res <-
      res %>% 
      dplyr::mutate(
        wt = idx / max(idx),
        # # Do these sequentially.
        dplyr::across(wt, ~dplyr::if_else(is.na(estimated_followers_count_a), .x * 0.5, .x)),
        dplyr::across(wt, ~dplyr::if_else(is.na(estimated_followers_count_h), .x * 0.5, .x)),
        # dplyr::across(wt, ~dplyr::if_else(is.na(estimated_followers_count_a) | is.na(estimated_followers_count_h), .x * 0.5, .x))
        dplyr::across(wt, ~.x^0.5)
      ) %>% 
      dplyr::relocate(idx, wt)
  }
  
  res
}

