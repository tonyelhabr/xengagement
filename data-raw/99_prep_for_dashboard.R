
library(tidyverse)
valid_stems <- get_valid_stems()
cols_lst <- get_cols_lst(valid_stems[1]) # Doesn't matter what the target variable is.
dir_data <- get_dir_data()

.path_data_x <- function(file, ext = NULL) {
  .path_data(file = sprintf('%s_%s', file, stem), ext = ext)
}
.path_data_rds <- purrr::partial(.path_data, ext = 'rds', ... = )

# ----
# Maybe this should be exported tweets_transformed (with `usethis::use_data()`)?
cols_x <- 
  dplyr::tibble(
    lab = c(cols_lst$cols_x_names, 'Baseline'),
    feature = c(cols_lst$cols_x, 'baseline')
  )
cols_x
readr::write_rds(cols_x, .path_data_rds(file = 'cols_x'))

# ----
.f_import_preds <- function(stem) {
  path <- file.path(dir_data, sprintf('preds_%s.parquet', stem))
  res <- 
    path %>% 
    arrow::read_parquet() %>% 
    dplyr::rename_with(~sprintf('%s_pred', stem), .cols = c(.pred)) %>% 
    dplyr::select(-dplyr::matches('_log$')) %>% 
    dplyr::mutate(
      dplyr::across(
        dplyr::matches(sprintf('^%s_', stem)),
        list(prnk = ~dplyr::percent_rank(.x))
      )
    )
  res
}

suppressMessages(
  preds <-
    valid_stems %>%
    purrr::map(.f_import_preds) %>%
    purrr::reduce(dplyr::full_join) %>%
    dplyr::select(
      dplyr::one_of(cols_lst$cols_id),
      dplyr::one_of(cols_lst$cols_extra),
      dplyr::matches('^(favorite|retweet)_')
    )
)
preds <-
  preds %>% 
  dplyr::mutate(
    favorite_diff = favorite_count - favorite_pred,
    retweet_diff = retweet_count - retweet_pred,
    dplyr::across(dplyr::matches('_diff$'), list(prnk = ~dplyr::percent_rank(.x))),
    dplyr::across(
      text,
      ~ sprintf(
        '%s H%02d (%s): %s (%.2f) %d-%d (%.2f) %s',
        lubridate::date(created_at),
        lubridate::hour(created_at),
        lubridate::wday(created_at, label = TRUE),
        tm_h,
        xg_h,
        g_h,
        g_a,
        xg_a,
        tm_a
      )
    )
  ) %>% 
  dplyr::arrange(idx)
preds %>% arrange(favorite_diff_prnk) %>% arrange(-favorite_diff_prnk) %>% select(matches('_diff'))
readr::write_rds(preds, .path_data_rds(file = 'preds'))

# shap ----
tweets_transformed <- file.path(dir_data, 'tweets_transformed.parquet') %>% arrow::read_parquet()
tweets_transformed

tweets_rescaled_long <-
  tweets_transformed %>% 
  dplyr::select(
    dplyr::all_of(cols_lst$cols_id),
    dplyr::any_of(cols_lst$cols_x)
  ) %>% 
  as.data.frame() %>% 
  dplyr::mutate(dplyr::across(-idx, scales::rescale)) %>% 
  tidyr::gather(
    'feature',
    'value',
    -c(idx)
  ) %>% 
  dplyr::as_tibble()
tweets_rescaled_long

.f_import_shap <- function(stem) {
  path <- file.path(dir_data, sprintf('shap_%s.parquet', stem))
  shap <- path %>% arrow::read_parquet()
  shap_long <-
    shap %>%
    tidyr::pivot_longer(
      -c(idx, .pred, .actual),
      names_to = 'feature',
      values_to = 'shap_value'
    ) %>%
    dplyr::mutate(sign = dplyr::if_else(shap_value < 0, 'neg', 'pos') %>% factor())
  shap_long

  res <-
    shap_long %>% 
    dplyr::full_join(
      tweets_rescaled_long, by = c('idx', 'feature')
    ) %>% 
    dplyr::left_join(cols_x, by = c('feature'))
  res
}

shap <-
  valid_stems %>%
  setNames(., .) %>% 
  purrr::map_dfr(.f_import_shap, .id = 'stem') %>% 
  dplyr::rename(pred = .pred, count = .actual) %>% 
  tidyr::pivot_wider(
    names_from = stem,
    values_from = c(pred, count, sign, shap_value),
    names_glue = '{stem}_{.value}'
  ) %>% 
  dplyr::mutate(dplyr::across(where(is.numeric), ~dplyr::coalesce(.x, 0))) %>% 
  dplyr::left_join(preds %>% dplyr::select(idx, text), by = 'idx') %>% 
  dplyr::filter(feature != 'baseline') %>% 
  dplyr::mutate(dplyr::across(text, ~forcats::fct_reorder(.x, dplyr::desc(.x)))) %>% 
  dplyr::arrange(idx, feature)
readr::write_rds(shap, .path_data_rds(file = 'shap'))


# ----
# TODO
mape_favorite <-
  preds %>% 
  yardstick::mape(favorite_pred, favorite_count)

mape_retweet <-
  preds %>% 
  yardstick::mape(retweet_pred, retweet_count)

mapes <- mape_favorite$.estimate + mape_retweet$.estimate
# Yes, flip the weights so that the more accurate one gets higher weighting
wt_favorite <- mape_retweet$.estimate / mapes
wt_retweet <- mape_favorite$.estimate / mapes

# # Need the acutal ranges? Or can use pranks of differences
# preds_agg <-
#   preds %>% 
#   dplyr::summarize(
#     dplyr::across(dplyr::matches('^(favorite|retweet)_'), range)
#   ) %>% 
#   dplyr::mutate(
#     idx = dplyr::row_number(),
#     stat = dplyr::if_else(idx == 1L, 'min', 'max')
#   ) %>% 
#   dplyr::select(-idx)
# preds_agg
# 
# .pull_wts <- function(.stem) {
#   wts %>% dplyr::filter(stem == .stem) %>% dplyr::pull(frac)
# }
# 
# res <-
#   preds %>% 
#   dplyr::mutate(
#     favorite_diff = (favorite_count - favorite_pred),
#     retweet_diff = (retweet_count - retweet_pred)
#   ) %>% 
#   # TODO: This is just an approximation for now. Should actually use a re-scaling function (with `preds_agg`).
#   dplyr::mutate(
#     retweet_count_scaled = 10 * retweet_count,
#     retweet_diff_scaled = 10 * retweet_diff
#   ) %>% 
#   dplyr::mutate(
#     total_diff = .pull_wts('favorite') * favorite_diff + .pull_wts('retweet') * retweet_diff_scaled,
#     total_diff_frac = total_diff / (favorite_count + retweet_count)
#   ) %>% 
#   dplyr::select(-dplyr::matches('_scaled$')) %>% 
#   dplyr::arrange(dplyr::desc(total_diff_frac))
# 
# readr::write_csv(res, file.path(dir_data, 'data_and_preds.csv'), na = '')
