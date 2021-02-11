
# NOTE: A lot of this is similar to what is in the update script. Not sure if there is a good way to avoid duplication. This script is intended for adhoc purposes, such as looking at predictions for individual teams, computing MAPE, etc.

valid_stems <- xengagement::get_valid_stems()
.path_data_rds <- purrr::partial(.path_data, ext = 'rds', ... = )
cols_lst <- xengagement::get_cols_lst(valid_stems[1])
n_hour_fresh <- getOption('xengagement.n_hour_fresh')

.f_import_preds <- function(stem) {
  path <- .path_data_rds(file = sprintf('preds_%s', stem))
  col_res_sym <- sprintf('%s_diff', stem) %>% dplyr::sym()
  col_pred_sym <- sprintf('%s_pred', stem) %>% dplyr::sym()
  res <- 
    path %>% 
    readr::read_rds() %>% 
    dplyr::rename_with(~sprintf('%s_pred', stem), .cols = c(.pred)) %>% 
    dplyr::select(-dplyr::matches('_log$')) %>% 
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
preds_init

preds_init <-
  preds_init %>% 
  dplyr::mutate(
    lab_text =
      sprintf(
        '%s: %s (%.2f) %d-%d (%.2f) %s',
        lubridate::date(created_at),
        # lubridate::hour(created_at),
        # lubridate::wday(created_at, label = TRUE),
        tm_h,
        xg_h,
        g_h,
        g_a,
        xg_a,
        tm_a
      ),
    lab_hover = stringr::str_remove(lab_text, '^.*[:]\\s')
  ) %>% 
  dplyr::arrange(idx)

# mapes <-
#   preds_init %>% 
#   dplyr::filter(favorite_count > 0 & retweet_count > 0 & favorite_pred > 0 & retweet_pred > 0) %>% 
#   dplyr::summarize(
#     mape_favorite = mean(abs((favorite_count - favorite_pred) / favorite_count), na.rm = TRUE),
#     mape_retweet = mean(abs((retweet_count - retweet_pred) / retweet_count), na.rm = TRUE)
#   ) %>% 
#   dplyr::mutate(
#     mapes = mape_favorite + mape_retweet,
#     wt_favorite = mape_retweet / mapes,
#     wt_retweet = mape_favorite / mapes
#   )
# mapes
# wt_favorite <- mapes$wt_favorite
# wt_retweet <- mapes$wt_retweet
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
scaling_factor

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
preds

.f_select <- function(suffix) {
  preds_init %>%
    dplyr::select(
      idx,
      status_id,
      tm = !!dplyr::sym(sprintf('tm_%s', suffix)),
      favorite_count,
      retweet_count,
      favorite_pred,
      retweet_pred
    ) %>%
    dplyr::mutate(side = !!suffix)
}

preds_long <-
  dplyr::bind_rows(.f_select('a'), .f_select('h')) %>% 
  tidyr::pivot_longer(
    # -matches('^(favorite|retweet)'),
    -c(idx, status_id, tm, side),
    names_to = c('stem', 'what'),
    names_pattern = '(favorite|retweet)_(count|pred)'
  ) %>%
  tidyr::pivot_wider(names_from = 'what', values_from = 'value') %>% 
  dplyr::mutate(dplyr::across(stem, ~ sprintf('%ss', .toupper1(.x))))
preds_long

# tms_filt <- c('Man City', 'Liverpool', 'Chelsea', 'Man United', 'Arsenal', 'Tottenham')
tms_filt <- 'Brighton'
viz_preds <-
  preds_long %>% 
  dplyr::filter(!(tm %in% tms_filt)) %>% 
  ggplot2::ggplot() +
  ggplot2::aes(x = pred, y = count) +
  ggplot2::geom_point(alpha = 1, color = 'grey80') +
  ggplot2::geom_point(
    data = preds_long %>% dplyr::filter(tm %in% tms_filt),
    ggplot2::aes(color = tm),
    show.legend = FALSE,
    size = 2,
    inherit.aes = TRUE
  ) +
  # ggplot2::geom_abline(ggplot2::aes(slope = 1, intercept = 0), size = 1, linetype = 2) +
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_continuous(labels = scales::comma) +
  ggplot2::facet_wrap(~stem, scales = 'free') +
  .theme() +
  ggplot2::guides(color = ggplot2::guide_legend('')) +
  ggplot2::labs(
    title = 'xGPhilosophy Tweet Engagement',
    x = 'Predicted', y = 'Actual'
  )
viz_preds

# xg1 <- 3.1
# g1 <- 2
# xg2 <- 0.9
# g2 <- 0
# (xg1 - xg2) - (g1 - g2) # typical outcome
# (xg2 - xg1) - (g1 - g2) # win the game, lose the xg
# (xg1 - xg2) - (g2 - g1) # lose the game, win the xg
# # 3.1 - 1.5, 2 - 0

cols_x <- 
  dplyr::tibble(
    lab = c(cols_lst$cols_x_names, 'Baseline'),
    feature = c(cols_lst$cols_x, 'baseline')
  )

token <- get_twitter_token()
tweets <- retrieve_tweets(method = 'since', token = token)
tweets_transformed <- tweets %>% transform_tweets(train = FALSE)
.display_info('Reduced {nrow(tweets)} tweets to {nrow(tweets_transformed)} transformed tweets.')
tweets_transformed

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
tweets_rescaled_long

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
  dplyr::filter(feature != 'baseline') %>% 
  dplyr::arrange(dplyr::all_of(cols_lst$cols_id), feature)
shap

shap_id_cols <- c(cols_lst$cols_id, 'lab')
shap_long <-
  shap %>% 
  dplyr::select(dplyr::all_of(shap_id_cols), dplyr::matches('_shap_value$')) %>% 
  tidyr::pivot_longer(
    -dplyr::all_of(shap_id_cols),
    names_to = 'stem',
    values_to = 'shap_value'
  ) %>% 
  dplyr::mutate(dplyr::across(stem, ~stringr::str_remove(.x, '_shap_value$'))) %>% 
  dplyr::mutate(dplyr::across(stem, ~ sprintf('x%ss', .toupper1(.x))))
shap_long

shap_long %>% 
  dplyr::filter(status_id == max(status_id)) %>% 
  dplyr::mutate(sign = ifelse(shap_value > 0, 'pos', 'neg')) %>% 
  dplyr::group_by(stem) %>% 
  dplyr::mutate(rnk = dplyr::row_number(dplyr::desc(abs(shap_value)))) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(rnk <= 10L) %>% 
  ggplot2::ggplot() +
  ggplot2::aes(y = tidytext::reorder_within(lab, shap_value, stem), x = shap_value) +
  ggplot2::geom_col(ggplot2::aes(fill = sign), show.legend = FALSE) +
  tidytext::scale_y_reordered() +
  ggplot2::scale_fill_manual(values = c('neg' = '#7a5193', 'pos' = '#ef5675')) +
  .theme(base_size = 12) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank()
  ) +
  ggplot2::facet_wrap(~stem, scales = 'free') +
  ggplot2::labs(
    title = '10 Most Important Factors for xEngagement',
    y = NULL,
    x = 'mean(|SHAP value|)'
  )

shap_agg <-
  shap_long %>% 
  dplyr::group_by(lab, stem) %>% 
  dplyr::summarize(dplyr::across(shap_value, ~mean(abs(.x)))) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(stem) %>% 
  dplyr::mutate(
    dplyr::across(shap_value, list(rnk = ~dplyr::row_number(dplyr::desc(.x))))
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(stem, shap_value_rnk)
shap_agg

viz_shap_agg <-
  shap_agg %>% 
  ggplot2::ggplot() +
  ggplot2::aes(y = tidytext::reorder_within(lab, shap_value, stem), x = shap_value) +
  ggplot2::geom_col() +
  tidytext::scale_y_reordered() +
  # .theme() +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank()
  ) +
  ggplot2::facet_wrap(~stem, scales = 'free') +
  ggplot2::labs(
    y = NULL,
    x = 'mean(|SHAP value|)'
  )
viz_shap_agg
