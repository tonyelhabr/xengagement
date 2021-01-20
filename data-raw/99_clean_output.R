
valid_stems <- get_valid_stems()
cols_lst <- get_cols_lst(valid_stems[1]) # Doesn't matter what the target variable is.
.f_import_pred <- function(stem) {
  path <- file.path(get_dir_data(), sprintf('preds_%s.parquet', stem))
  res <- 
    path %>% 
    arrow::read_parquet() %>% 
    dplyr::rename_with(~sprintf('%s_pred', stem), .cols = c(.pred)) %>% 
    dplyr::select(-dplyr::matches('_log$'))
  res
}

.f_import_shap <- function(stem) {
  path <- file.path(get_dir_data(), sprintf('shap_%s.parquet', stem))
  res <- 
    path %>% 
    arrow::read_parquet() # %>% 
    # dplyr::rename_with(~sprintf('%s_pred', stem), .cols = c(.pred)) %>% 
    # dplyr::select(-dplyr::matches('_log$'))
  res
}

shap <-
  valid_stems %>%
  purrr::map(.f_import_shap) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  # dplyr::select(
  #   dplyr::one_of(cols_lst$cols_id),
  #   dplyr::one_of(cols_lst$cols_extra),
  #   favorite_pred,
  #   retweet_pred
  # ) %>%
  dplyr::arrange(idx)
shap

suppressMessages(
  preds <-
    valid_stems %>%
    purrr::map(.f_import_preds) %>%
    purrr::reduce(dplyr::full_join) %>%
    dplyr::select(
      dplyr::one_of(cols_lst$cols_id),
      dplyr::one_of(cols_lst$cols_extra),
      favorite_pred,
      retweet_pred
    ) %>%
    dplyr::arrange(idx)
)
preds

.compute_ccc <- function(stem) {
  col_actual <- sprintf('%s_count', stem)
  col_pred <- sprintf('%s_pred', stem)
  res <-
    preds %>% 
    yardstick::ccc(!!sym(col_actual), !!sym(col_pred)) %>% 
    dplyr::pull(.estimate)
  dplyr::tibble(stem = !!stem, metric = 'ccc', .estimate = res)
}

preds_agg <-
  preds %>% 
  dplyr::summarize(
    dplyr::across(dplyr::matches('^(favorite|retweet)_'), range)
  ) %>% 
  dplyr::mutate(
    idx = dplyr::row_number(),
    stat = dplyr::if_else(idx == 1L, 'min', 'max')
  ) %>% 
  dplyr::select(-idx)
preds_agg

wts <-
  valid_stems %>% 
  purrr::map_dfr(.compute_ccc) %>% 
  dplyr::group_by(metric) %>% 
  dplyr::mutate(
    frac = .estimate^2 / sum(.estimate^2)
  ) %>% 
  dplyr::ungroup()
wts

.pull_wts <- function(.stem) {
  wts %>% dplyr::filter(stem == .stem) %>% dplyr::pull(frac)
}

res <-
  preds %>% 
  dplyr::mutate(
    favorite_diff = (favorite_count - favorite_pred),
    retweet_diff = (retweet_count - retweet_pred)
  ) %>% 
  # TODO: This is just an approximation for now. Should actually use a re-scaling function (with `preds_agg`).
  dplyr::mutate(
    retweet_count_scaled = 10 * retweet_count,
    retweet_diff_scaled = 10 * retweet_diff
  ) %>% 
  dplyr::mutate(
    total_diff = .pull_wts('favorite') * favorite_diff + .pull_wts('retweet') * retweet_diff_scaled,
    total_diff_frac = total_diff / (favorite_count + retweet_count)
  ) %>% 
  dplyr::select(-dplyr::matches('_scaled$')) %>% 
  dplyr::arrange(dplyr::desc(total_diff_frac))
# res %>% dplyr::arrange(dplyr::desc(total_diff))
# res %>% dplyr::arrange(dplyr::desc(total_diff_frac))
# res
# res %>% 
#   ggplot() +
#   aes(x = total_diff, y = total_diff_frac) + 
#   geom_point()
# 
# res %>% 
#   ggplot() +
#   aes(x = created_at, y = total_diff) +
#   geom_point()
# res %>% 
#   ggplot() +
#   aes(x = created_at, y = favorite_count) +
#   geom_point()
readr::write_csv(res, 'xengagement.csv', na = '')
