
# setup ----
library(xengagement)
# library(tweetrmd)

options(xengagement.dir_data = 'inst/extdata')
dir_figs <- 'inst/extdata'

token <- xengagement::get_twitter_token()
dir_data <- xengagement::get_dir_data()
valid_stems <- xengagement::get_valid_stems()
# Doesn't matter what the target variable is currently cuz the dashboard doesn't use it.
cols_lst <- xengagement::get_cols_lst(valid_stems[1])
n_hour_fresh <- getOption('xengagement.n_hour_fresh')

paths_data <- list.files(dir_data, full.names = TRUE)
paths_data_info <- file.info(paths_data)
cat(sprintf('Files in `dir_data = "%s"`.', dir_data), sep = '\n')
# cbind(as.data.frame(paths_data), paths_data_info[, c('mtime')])

# functions, utils-display ----
.display_info <- function(x, ..., .envir = parent.frame(), .verbose = TRUE, .f_glue = glue::glue_collapse) {
  if (!.verbose) {
    return(invisible(x))
  }
  x <- .f_glue(x, '\n')
  x <- glue::glue(x, .envir = .envir)
  cat(x, sep = '\n')
}

.display_warning <- function(x, ..., .envir = parent.frame()) {
  x <- glue::glue_collapse(x, '\n')
  x <- glue::glue(x, .envir = .envir)
  warning(x, call. = FALSE, immediate. = TRUE)
}

.display_error <- function(x, ..., .envir = parent.frame()) {
  x <- glue::glue_collapse(x, '\n')
  x <- glue::glue(x, .envir = .envir)
  cnd <- structure(class = c('usethis_error', 'error', 'condition'), list(message = x))
  stop(cnd)
}

# functions, path ----
.path_x <- function(dir, file = tempfile(), ext = NULL) {
  if(!is.null(ext)) {
    ext <- sprintf('.%s', ext)
  } else {
    ext <- ''
  }
  file.path(dir, sprintf('%s%s', file, ext))
}

.path_data <- function(dir = dir_data, ...) {
  .path_x(dir = dir, ...)
}
# .path_data_csv <- purrr::partial(.path_data, ext = 'csv', ... = )
.path_data_rds <- purrr::partial(.path_data, ext = 'rds', ... = )

.export_csv <- function(x, file = deparse(substitute(x)), na = '', ...) {
  readr::write_csv(x, .path_data(file = file, ext = 'csv'), na = na, ...)
}

# functions, main ----
do_update <- function() {
  
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
    )
  
  is_null <- is.null(tweets_new)
  if(is_null) {
    suffix <- ifelse(n_hour_fresh > 1L, 's', '')
    .display_info('0 new tweets found in past {n_hour_fresh} hours{suffix} at {Sys.time()}!')
    return(invisible(FALSE))
  }
  n_tweet <- nrow(tweets_new)
  
  tweets <-
    xengagement::retrieve_tweets(
      method = 'all',
      export = TRUE,
      token = token
    )
  
  .f_transform <- function() {
    
    tweets_transformed <- tweets %>% xengagement::transform_tweets(train = FALSE)
    .display_info('Reduced {nrow(tweets)} tweets to {nrow(tweets_transformed)} after transformation.')
    tweets_transformed
  }
  
  tweets_transformed <-
    xengagement::do_get(
      f = .f_transform,
      path = .path_data_rds(file = 'tweets_transformed'),
      f_import = readr::read_rds,
      f_export = readr::write_rds,
      overwrite = TRUE,
      export = TRUE
    )
  
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
  
  mapes <-
    preds_init %>% 
    dplyr::filter(favorite_count > 0 & retweet_count > 0 & favorite_pred > 0 & retweet_pred > 0) %>% 
    dplyr::summarize(
      mape_favorite = mean(abs((favorite_count - favorite_pred) / favorite_count), na.rm = TRUE),
      mape_retweet = mean(abs((retweet_count - retweet_pred) / retweet_count), na.rm = TRUE)
    ) %>% 
    dplyr::mutate(
      mapes = mape_favorite + mape_retweet,
      wt_favorite = mape_retweet / mapes,
      wt_retweet = mape_favorite / mapes
    )
  wt_favorite <- mapes$wt_favorite
  wt_retweet <- mapes$wt_retweet
  
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
      names_pattern = '(favorite|retweet)_(count|pred)'
    ) %>%
    tidyr::pivot_wider(names_from = 'what', values_from = 'value')
  
  # UPDATE: Fixed, but not currently using the outputs, so don't run for now.
  if(FALSE) {
    # This is a valid way as well. It just isn't as clear what's going on.
    # res_screenshot <- preds %>% xengagement::screenshot_latest_tweet(dir = dir_figs)
    latest_tweet <- preds %>% dplyr::slice_max(created_at, with_ties = FALSE)
    .f_screenshot <- 
      purrr::partial(
        xengagement::screenshot_latest_tweet, 
        dir = dir_figs,
        ... = 
      )
    res_screenshot <- .f_screenshot(status_id = latest_tweet$status_id)
    
    latest_tweet_bot <- tweets_bot %>% dplyr::slice_max(created_at, with_ties = FALSE)
    res_screenshot_bot <- .f_screenshot(status_id = latest_tweet_bot$status_id)
  }
  
  res_generate <-
    preds %>%
    dplyr::semi_join(tweets_new %>% dplyr::select(status_id), by = 'status_id') %>% 
    tidyr::nest(data = -c(idx)) %>%
    dplyr::mutate(res = purrr::map(
      data,
      ~ xengagement::generate_tweet(
        pred = .x,
        tweets = tweets_bot,
        in_reply_to_tweets = tweets,
        # in_reply_to_status_id = ..2,
        preds_long = preds_long,
        dir = dir_figs,
        dry_run = FALSE
      )
    ))
  
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
    dplyr::mutate(dplyr::across(-idx, scales::rescale)) %>% 
    tidyr::gather(
      'feature',
      'value',
      -c(idx)
    ) %>% 
    dplyr::as_tibble()
  
  .f_import_shap <- function(stem) {
    path <- .path_data_rds(file = sprintf('shap_%s', stem))
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
    dplyr::left_join(preds %>% dplyr::select(idx, lab_text), by = 'idx') %>% 
    dplyr::filter(feature != 'baseline') %>% 
    dplyr::arrange(idx, feature)
  
  .export_csv(preds)
  .export_csv(shap)
  
  .display_info('Successfully completed update at {Sys.time()}.')
  return(invisible(TRUE))
}

# main ----
print(paths_data_info[, c('mtime'), drop = FALSE])
do_update()
