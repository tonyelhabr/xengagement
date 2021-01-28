
library(xengagement)
options(xengagement.dir_data = 'inst/extdata')
dir_figs <- 'inst/extdata'
token <- xengagement::get_twitter_token()
dir_data <- xengagement::get_dir_data()
valid_stems <- xengagement::get_valid_stems()
cols_lst <- xengagement::get_cols_lst(valid_stems[1]) # Doesn't matter what the target variable is.

# data refresh ----
tweets_bot <-
  xengagement::retrieve_tweets(
    user = 'punditratio',
    method = 'since',
    export = TRUE,
    token = token
  )
tweets_bot

tweets_new <-
  xengagement::retrieve_tweets(
    method = 'new',
    export = FALSE,
    token = token
  )
tweets_new

is_null <- is.null(tweets_new)
if(is_null) {
  cat(sprintf('0 new tweet found at %s!', Sys.time()), , sep = '\n')
} else {
  n_tweet <- nrow(tweets_new)
  cat(sprintf('%d new tweet%s found at %s!', n_tweet, ifelse(n_tweet > 1L, 's', ''), Sys.time()), sep = '\n')
  
  # .f_transform <- function() {
  #   tweets <- xengagement::retrieve_tweets(method = 'since', export = TRUE, append = TRUE, token = token)
  #   tweets_transformed <- tweets %>% xengagement::transform_tweets(train = FALSE)
  #   cat(sprintf('Reduced %s tweets to %s after transformation.', nrow(tweets), nrow(tweets_transformed)))
  #   tweets_transformed
  # }
  # 
  # tweets_transformed <-
  #   xengagement::do_get(
  #     f = .f_transform,
  #     path = file.path(dir_data, 'tweets_transformed.rds'),
  #     f_import = readr::read_rds,
  #     f_export = readr::write_rds,
  #     overwrite = TRUE,
  #     export = TRUE
  #   )
  # tweets_transformed
  
  tweets <-
    xengagement::retrieve_tweets(
      tweets = tweets_new,
      method = 'none',
      export = TRUE,
      token = token
    )
  tweets_transformed <- tweets %>% xengagement::transform_tweets(train = FALSE)
  cat(sprintf('Reduced %s tweets to %s after transformation.', nrow(tweets), nrow(tweets_transformed)), sep = '\n')
  tweets_transformed
  
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
  res_preds
  
  # dashboard prep ---
  .path_x <- function(dir, file = tempfile(), ext = NULL) {
    if(!is.null(ext)) {
      ext <- sprintf('.%s', ext)
    } else {
      ext <- ''
    }
    file.path(dir, sprintf('%s%s', file, ext))
  }
  
  .path_data <- function(dir = get_dir_data(), ...) {
    .path_x(dir = dir, ...)
  }
  .path_data_csv <- purrr::partial(.path_data, ext = 'csv', ... = )
  
  # preds export ----
  .f_import_preds <- function(stem) {
    path <- file.path(dir_data, sprintf('preds_%s.rds', stem))
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
  
  preds_init <-
    preds_init %>% 
    dplyr::mutate(
      dplyr::across(
        text,
        ~ sprintf(
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
        )
      ),
      lab_hover = 
        sprintf(
          '%s (%.2f) %d-%d (%.2f) %s',
          tm_h,
          xg_h,
          g_h,
          g_a,
          xg_a,
          tm_a
        )
    ) %>% 
    dplyr::arrange(idx)
  preds_init
  
  mapes <-
    preds_init %>% 
    dplyr::filter(retweet_count > 0) %>% 
    dplyr::summarize(
      mape_favorite = mean(abs((favorite_count - favorite_pred) / favorite_count), na.rm = TRUE),
      mape_retweet = mean(abs((retweet_count - retweet_pred) / retweet_count), na.rm = TRUE)
    ) %>% 
    dplyr::mutate(
      mapes = mape_favorite + mape_retweet,
      wt_favorite = mape_retweet / mapes,
      wt_retweet = mape_favorite / mapes
    )
  mapes
  wt_favorite <- mapes$wt_favorite
  wt_retweet <- mapes$wt_retweet
  
  preds_agg <-
    preds_init %>%
    dplyr::summarize(
      dplyr::across(
        dplyr::matches('^(favorite|retweet)_(count)$'),
        list(min = min, max = max)
      )
    )
  preds_agg
  
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
  # preds %>% dplyr::select(total_diff_prnk, total_diff, text, tm_h, tm_a) %>% dplyr::arrange(total_diff_prnk)

  latest_tweet <-
    preds %>%
    dplyr::slice_max(created_at)
  
  res_screenshot <-
    preds %>% 
    xengagement::screenshot_latest_tweet(dir = dir_figs)
  
  res_generate <-
    preds %>%
    dplyr::semi_join(tweets_new %>% dplyr::select(status_id), by = 'status_id') %>% 
    tidyr::nest(data = -c(idx, status_id)) %>%
    dplyr::mutate(res = purrr::map2(
      data, status_id,
      ~ xengagement::generate_tweet(
        pred = ..1,
        tweets = tweets_bot,
        in_reply_to_status_id = ..2,
        dry_run = TRUE
      )
    ))
  res_generate
  readr::write_csv(preds, file.path(dir_data, 'preds.csv'), na = '')
  
  # shap export ----
  # Maybe this should be exported tweets_transformed (with `usethis::use_data()`)?
  cols_x <- 
    dplyr::tibble(
      lab = c(cols_lst$cols_x_names, 'Baseline'),
      feature = c(cols_lst$cols_x, 'baseline')
    )
  cols_x
  
  # tweets_transformed <- file.path(dir_data, 'tweets_transformed.rds') %>% readr::read_rds()
  # tweets_transformed
  
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
    path <- file.path(dir_data, sprintf('shap_%s.rds', stem))
    shap <- path %>% readr::read_rds()
    shap_long <-
      shap %>%
      tidyr::pivot_longer(
        -c(idx, .pred, .actual),
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
    dplyr::mutate(dplyr::across(sign, as.character)) %>% 
    tidyr::pivot_wider(
      names_from = stem,
      values_from = c(pred, count, sign, shap_value),
      names_glue = '{stem}_{.value}',
      values_fill = list(pred = 0, count = 0, sign = 'neutral', shap_value = 0)
    ) %>% 
    # dplyr::mutate(dplyr::across(where(is.numeric), ~dplyr::coalesce(.x, 0))) %>% 
    dplyr::left_join(preds %>% dplyr::select(idx, text), by = 'idx') %>% 
    dplyr::filter(feature != 'baseline') %>% 
    # dplyr::mutate(dplyr::across(text, ~forcats::fct_reorder(.x, dplyr::desc(.x)))) %>% 
    dplyr::arrange(idx, feature)
  shap
  readr::write_csv(shap, .path_data_csv(file = 'shap'))
  
  cat(sprintf('Successfully completed update at %s', Sys.time()), sep = '\n')
}
