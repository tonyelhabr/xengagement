
# setup ----
library(xengagement)
# library(tweetrmd)

options(xengagement.dir_data = 'inst/extdata')
dir_figs <- 'inst/extdata'

token <- get_twitter_token()
dir_data <- get_dir_data()
valid_stems <- get_valid_stems()
# Doesn't matter what the target variable is currently cuz the dashboard doesn't use it.
cols_lst <- get_cols_lst(valid_stems[1])
n_hour_fresh <- getOption('xengagement.n_hour_fresh')

paths_data <- list.files(dir_data, full.names = TRUE)
paths_data_info <- file.info(paths_data)
cat(sprintf('Files in `dir_data = "%s"`.', dir_data), sep = '\n')
# cbind(data.frame('path' = paths_data), data.frame('time' = paths_data_info[, c('mtime')]))
# print(paths_data_info[, c('mtime'), drop = FALSE])

# functions, utils-display ----
# Rename this from `.display_info()` for the sake of not having the same function name as the package.
.inform <- function(x, ..., .envir = parent.frame(), .verbose = TRUE, .f_glue = glue::glue_collapse) {
  if (!.verbose) {
    return(invisible(x))
  }
  x <- .f_glue(x, '\n')
  x <- glue::glue(x, .envir = .envir)
  cat(x, sep = '\n')
}

# functions, path ----
# .path_data_csv <- purrr::partial(.path_data, ext = 'csv', ... = )
.path_data_rds <- purrr::partial(.path_data, ext = 'rds', ... = )

.export_csv <- function(x, file = deparse(substitute(x)), na = '', ...) {
  readr::write_csv(x, .path_data(file = file, ext = 'csv'), na = na, ...)
}

# main ----
tweets_bot <-
  xengagement::retrieve_tweets(
    user = 'punditratio',
    method = 'since',
    export = TRUE,
    token = token
  )

tweets_new <-
  xengagement::retrieve_tweets(
    user = 'xGPhilosophy',
    method = 'new',
    export = FALSE,
    token = token
  ) %>% 
  dplyr::filter(!is_retweet & !is_quote & is.na(reply_to_status_id))

is_null <- is.null(tweets_new)
if(is_null) {
  suffix <- ifelse(n_hour_fresh > 1L, 's', '')
  .inform('0 new tweets found in past {n_hour_fresh} hours{suffix} at {Sys.time()}!')
  return(invisible(FALSE))
}
n_tweet <- nrow(tweets_new)

tweets <-
  xengagement::retrieve_tweets(
    method = 'all',
    export = TRUE,
    token = token
  )

tweets_transformed <- tweets %>% transform_tweets(train = FALSE)
.inform('Reduced {nrow(tweets)} tweets to {nrow(tweets_transformed)} transformed tweets.')

res_preds <-
  dplyr::tibble(
    stem = valid_stems,
    fit = list(xengagement::fit_favorite, xengagement::fit_retweet)
  ) %>% 
  dplyr::mutate(
    res = purrr::map2(
      stem, fit,
      ~ xengagement::do_predict(
        tweets_transformed = tweets_transformed,
        stem = ..1,
        fit = ..2,
        .overwrite = list(preds = TRUE, shap = TRUE)
      )
    )
  )

.f_import_preds <- function(stem) {
  path <- .path_data_rds(file = sprintf('preds_%s', stem))
  col_res_sym <- sprintf('%s_diff', stem) %>% dplyr::sym()
  col_pred_sym <- sprintf('%s_pred', stem) %>% dplyr::sym()
  res <- 
    path %>% 
    readr::read_rds() %>% 
    dplyr::rename_with(~sprintf('%s_pred', stem), .cols = c(.pred)) %>% 
    dplyr::select(-dplyr::matches('_trans$')) %>% 
    dplyr::mutate(
      !!col_res_sym := !!dplyr::sym(sprintf('%s_count', stem)) - !!col_pred_sym,
    ) %>% 
    dplyr::mutate(
      dplyr::across(
        c(!!col_pred_sym, !!col_res_sym),
        list(prnk = ~dplyr::percent_rank(.x))
      )
    )
  res
}

suppressMessages(
  preds_init <-
    valid_stems %>%
    purrr::map(.f_import_preds) %>%
    purrr::reduce(dplyr::full_join) %>%
    dplyr::select(
      dplyr::one_of(cols_lst$cols_id),
      dplyr::one_of(cols_lst$cols_extra),
      dplyr::matches('^(favorite|retweet)_')
    )
)

preds_init <-
  preds_init %>% 
  dplyr::mutate(
    lab_text =
      sprintf(
        '%s: %s (%.2f) %d-%d (%.2f) %s',
        lubridate::date(created_at),
        team_h,
        xg_h,
        g_h,
        g_a,
        xg_a,
        team_a
      )
  ) %>% 
  dplyr::arrange(idx)

wt_favorite <- 0.5
wt_retweet <- 0.5

now <- lubridate::now()

preds_agg <-
  preds_init %>%
  dplyr::filter(created_at <= (!!now - lubridate::hours(n_hour_fresh))) %>% 
  dplyr::summarize(
    dplyr::across(
      dplyr::matches('^(favorite|retweet)_(count)$'),
      list(min = min, max = max)
    )
  )

scaling_factor <-
  preds_agg %>% 
  dplyr::transmute(
    scaling_factor = (favorite_count_max - favorite_count_min) / (retweet_count_max - retweet_count_min)
  ) %>% 
  dplyr::pull(scaling_factor)

preds <-
  preds_init %>%
  dplyr::mutate(
    retweet_diff_scaled = !!scaling_factor * retweet_diff
  ) %>%
  dplyr::mutate(
    total_diff = !!wt_favorite * favorite_diff + !!wt_retweet * retweet_diff_scaled,
    dplyr::across(total_diff, list(prnk = ~dplyr::percent_rank(.x), rnk = ~dplyr::row_number(dplyr::desc(.x))))
  ) %>%
  dplyr::select(-dplyr::matches('_scaled$')) %>% 
  dplyr::arrange(total_diff_rnk)

# For tweeted viz.
.toupper1 <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

.mutate_stem_col <- function(data) {
  data %>% 
    dplyr::mutate(dplyr::across(stem, ~dplyr::case_when(.x == 'favorite' ~ 'Fav', .x == 'retweet' ~ 'RT') %>% paste0('s')))
}

preds_long <-
  preds %>%
  dplyr::select(
    status_id,
    favorite_count,
    favorite_pred,
    retweet_count,
    retweet_pred
  ) %>%
  tidyr::pivot_longer(
    -status_id,
    names_to = c('stem', 'what'),
    names_pattern = '^(favorite|retweet)_(count|pred)'
  ) %>%
  tidyr::pivot_wider(names_from = 'what', values_from = 'value') %>% 
  .mutate_stem_col()

# For by team viz.
.f_select <- function(suffix) {
  preds %>%
    dplyr::select(
      idx,
      status_id,
      created_at,
      team = !!dplyr::sym(sprintf('team_%s', suffix)),
      favorite_count,
      retweet_count,
      favorite_pred,
      retweet_pred
    ) %>%
    dplyr::mutate(side = !!suffix)
}

preds_by_team <-
  dplyr::bind_rows(.f_select('a'), .f_select('h')) %>%
  dplyr::left_join(
    preds %>% 
      dplyr::select(
        dplyr::all_of(cols_lst$cols_id),
        lab_text,
        dplyr::matches('^total_diff')
      )
  )

cols_x <- 
  dplyr::tibble(
    lab = c(cols_lst$cols_x_names, 'Baseline'),
    feature = c(cols_lst$cols_x, 'baseline')
  )

tweets_rescaled_long <-
  tweets_transformed %>% 
  dplyr::select(
    dplyr::all_of(cols_lst$cols_id),
    dplyr::any_of(cols_lst$cols_x)
  ) %>% 
  as.data.frame() %>% 
  dplyr::mutate(
    dplyr::across(
      -dplyr::all_of(cols_lst$cols_id), 
      scales::rescale
    )
  ) %>% 
  tidyr::gather(
    'feature',
    'value',
    -c(cols_lst$cols_id)
  ) %>% 
  dplyr::as_tibble()

.f_import_shap <- function(stem) {
  path <- .path_data_rds(file = sprintf('shap_%s', stem))
  shap <- path %>% readr::read_rds()
  shap_long <-
    shap %>%
    tidyr::pivot_longer(
      -c(status_id, .pred, .actual),
      names_to = 'feature',
      values_to = 'shap_value'
    ) %>%
    dplyr::mutate(
      sign = 
        dplyr::case_when(
          shap_value < 0 ~ 'neg', 
          shap_value > 0 ~ 'pos',
          TRUE ~ 'neutral'
        )
    )
  
  res <-
    shap_long %>% 
    dplyr::full_join(
      tweets_rescaled_long, by = c(cols_lst$cols_id, 'feature')
    ) %>% 
    dplyr::left_join(cols_x, by = c('feature'))
  res
}

cols_id <- c(cols_lst$cols_id, 'created_at')
shap <-
  valid_stems %>%
  stats::setNames(., .) %>% 
  purrr::map_dfr(.f_import_shap, .id = 'stem') %>% 
  dplyr::rename(pred = .pred, count = .actual) %>% 
  dplyr::mutate(dplyr::across(sign, as.character)) %>% 
  tidyr::pivot_wider(
    names_from = stem,
    values_from = c(pred, count, sign, shap_value),
    names_glue = '{stem}_{.value}',
    values_fill = list(pred = 0, count = 0, sign = 'neutral', shap_value = 0)
  ) %>% 
  dplyr::left_join(
    preds %>% 
      dplyr::select(dplyr::all_of(cols_id), lab_text), 
    by = cols_lst$cols_id
  ) %>% 
  dplyr::filter(feature != 'baseline') %>% 
  dplyr::arrange(dplyr::all_of(cols_lst$cols_id), feature)
shap

shap_id_cols <- c(cols_id, 'lab')
shap_long <-
  shap %>% 
  dplyr::select(dplyr::all_of(shap_id_cols), dplyr::matches('_shap_value$')) %>% 
  tidyr::pivot_longer(
    -dplyr::all_of(shap_id_cols),
    names_to = 'stem',
    values_to = 'shap_value'
  ) %>% 
  dplyr::mutate(dplyr::across(stem, ~stringr::str_remove(.x, '_shap_value$'))) %>% 
  .mutate_stem_col()
.export_csv(preds)
.export_csv(preds_by_team)
.export_csv(shap)

res_generate <-
  preds %>%
  dplyr::semi_join(
    tweets_new %>% dplyr::select(status_id), by = 'status_id'
  ) %>%
  tidyr::nest(data = -c(idx)) %>%
  dplyr::mutate(res = purrr::map(
    data,
    ~ xengagement::generate_tweet(
      pred = .x,
      tweets = tweets_bot,
      in_reply_to_tweets = tweets,
      preds = preds,
      preds_long = preds_long,
      shap_long = shap_long,
      dir = dir_figs,
      # delete_plot = TRUE,
      dry_run = FALSE
    )
  ))

.inform('Successfully completed update at {Sys.time()}.')

