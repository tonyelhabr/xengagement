
#' Make predictions
#' 
#' Make predictions for new tweets, appending to existing predictions and SHAP values.
#' @inheritParams do_fit_model
#' @param ... Extra arguments to pass to `.transform_tweets()`
predict_new <- 
  function(tweets, 
           stem = get_valid_stems(), 
           overwrite = TRUE, 
           ...,
           .overwrite = list(
             preds = overwrite,
             shap = overwrite
           )) {
    .validate_stem(stem)
    cols_lst <- .get_cols_lst(stem = stem)
    data <- tweets %>% .transform_tweets(train = FALSE, ...)
    
    .path_data_x <- function(file, ext = NULL) {
      .path_data(file = sprintf('%s_%s', file, stem), ext = ext)
    }
    path_fit <- .path_data_x('fit')
    path_preds <- .path_data_parquet_x('preds')
    path_shap_wide <- .path_data_parquet_x('shap_wide')
    
    fit <- xgboost::xgb.load(path_fit)
    
    col_y_sym <- cols_lst$col_y %>% sym()
    x_mat <- data %>% dplyr::select(dplyr::one_of(c(cols_lst$cols_x))) %>% .df2mat()
    
    .f_predict <- function() {
      
      preds <-
        fit %>%
        stats::predict(x_mat) %>%
        .augment_preds(
          data = data,
          cols_id = cols_lst$cols_id,
          cols_extra = cols_lst$cols_extra,
          col_y = cols_lst$col_y,
          f_trans = .inverse_log
        )
      preds
    }
    
    preds <- 
      .get_x(
        f = .f_predict,
        path = path_preds,
        f_import = arrow::read_parquet,
        f_export = arrow::write_parquet,
        append = TRUE,
        export = TRUE,
        overwrite = .overwrite$preds
      )
    
    .f_shap_wide <- function() {
      .shap_xgb(
        data = data,
        x_mat = x_mat,
        fit = fit,
        preds = preds
      )
    }
    
    shap_wide <-
      .get_x(
        f = .f_shap_wide,
        path = path_shap_wide,
        f_import = arrow::read_parquet,
        f_export = arrow::write_parquet,
        append = TRUE,
        export = TRUE,
        overwrite = .overwrite$shap
      )
    preds
  }

#' Fit model
#' 
#' Fit model, make predictions, and generate SHAP values.
#' @param tweets Tweets from `import_tweets()`
#' @param stem Either `"favorite"` or `"retweet"`
#' @param overwrite Whether to overwrite existing fit, predictions, and SHAP values.
#' @param ... Extra arguments to pass to `.transform_tweets()`
#' @param .overwrite Specific booleans for overwriting specific outputs saved to file. Default is to use same value as `overwrite`.
do_fit_model <-
  function(tweets,
           stem = get_valid_stems(),
           overwrite = TRUE, 
           ...,
           .overwrite = list(
             tune = overwrite,
             fit = overwrite,
             preds = overwrite,
             shap = overwrite
           )) {
    
    # stem = 'favorite'
    # overwrite = TRUE
    # .overwrite = list(
    #   tune = overwrite,
    #   fit = overwrite,
    #   preds = overwrite,
    #   shap = overwrite
    # )
    
    .validate_stem(stem)
    cols_lst <- .get_cols_lst(stem = stem)
    # data <- tweets %>% .transform_tweets(train = TRUE, ...)
    data <- tweets %>% .transform_tweets(train = TRUE) # For running interactively.
    
    .path_data_x <- function(file, ext = NULL) {
      .path_data(file = sprintf('%s_%s', file, stem), ext = ext)
    }
    .path_data_parquet_x <- purrr::partial(.path_data_x, ext = 'parquet', ... = )
    
    # TODO: Make these arguments to the function, setting them to `NULL` by default.
    path_res_tune_cv <- .path_data_x('res_tune_cv', ext = 'rds')
    path_fit <- .path_data_x('fit')
    path_preds <- .path_data_parquet_x('preds')
    path_shap_wide <- .path_data_parquet_x('shap')
    
    col_y_sym <- cols_lst$col_y %>% sym()
    data <- data %>% tidyr::drop_na(!!col_y_sym)
    
    x_mat <- data %>% dplyr::select(dplyr::one_of(c(cols_lst$cols_x))) %>% .df2mat()
    
    # TODO: Make these package options?
    nrounds <- 2000
    booster <- 'gbtree'
    objective <- 'reg:squarederror'
    eval_metrics <- list('rmse')
    early_stopping_rounds <- 10
    print_every_n <- 100
    n_fold <- 10
    
    x_dmat <-
      xgboost::xgb.DMatrix(
        x_mat,
        label = data[[cols_lst$col_y]]
      )
    x_dmat
    
    .f_tune <- function() {
      
      seed <- .get_xengagement_seed()
      set.seed(seed)
      
      folds_ids <-
        .create_folds(
          data[[cols_lst$col_strata]],
          k = n_fold,
          list = FALSE,
          returnTrain = FALSE
        )
      folds_ids
      
      col_strata <- cols_lst$col_strata
      col_strata_sym <- col_strata %>% sym()
      folds <-
        data %>%
        dplyr::bind_cols(dplyr::tibble(fold = folds_ids)) %>%
        dplyr::left_join(
          data %>% dplyr::select(!!col_strata_sym, idx),
          by = c('idx', col_strata)
        ) %>%
        dplyr::select(fold, idx) %>%
        split(.$fold) %>%
        purrr::map(~dplyr::select(.x, -fold) %>% dplyr::pull(idx))
      folds
      n_obs <- folds %>% purrr::flatten_int() %>% length()
      max_idx <- folds %>% purrr::flatten_int() %>% max()
      assertthat::assert_that(n_obs == max_idx)
      
      n_row <- 50
      grid_params <-
        dials::grid_latin_hypercube(
          dials::finalize(dials::mtry(), data),
          dials::min_n(),
          dials::tree_depth(),
          dials::learn_rate(),
          dials::loss_reduction(),
          sample_size = dials::sample_prop(),
          size = n_row
        ) %>%
        dplyr::mutate(
          learn_rate = 0.1 * ((1:n_row) / n_row),
          mtry = mtry / ncol(data),
          idx = dplyr::row_number()
        ) %>%
        dplyr::relocate(idx)
      grid_params
      
      res_tune_cv <- 
        .tune_xgb_cv(
          nrounds = nrounds,
          stem = stem,
          grid_params = grid_params,
          folds = folds,
          x_dmat = x_dmat,
          booster = booster,
          objective = objective,
          eval_metrics = eval_metrics,
          sample_weight = data[[cols_lst$col_wt]],
          early_stopping_rounds = early_stopping_rounds,
          print_every_n = print_every_n
        )
      res_tune_cv
    }
    
    res_tune_cv <- 
      .get_x(
        f = .f_tune, 
        path = path_res_tune_cv, 
        f_import = readr::read_rds,
        f_export = readr::write_rds,
        append = FALSE,
        export = TRUE,
        overwrite = .overwrite$tune
      )
    
    .f_fit <- function() {
      eval_metric <- eval_metrics[1]
      eval_metric_tst <- sprintf('%s_tst', eval_metric)
      eval_metric_tst_sym <- eval_metric_tst %>% sym()
      res_cv_best <- res_tune_cv %>% dplyr::slice_min(!!eval_metric_tst_sym)
      res_cv_best
      
      .pluck_param <- function(x) {
        res_cv_best %>% purrr::pluck(x)
      }
      
      params_best <-
        list(
          booster = booster,
          objective = objective,
          eval_metric = eval_metrics,
          eta = .pluck_param('eta'),
          gamma = .pluck_param('gamma'),
          subsample = .pluck_param('subsample'),
          colsample_bytree = .pluck_param('colsample_bytree'),
          max_depth = .pluck_param('max_depth'),
          min_child_weight = .pluck_param('min_child_weight')
        )
      params_best
      
      nrounds_best <- round((.pluck_param('iter') / ((n_fold - 1) / (n_fold))), 0) + early_stopping_rounds
      
      fit <-
        xgboost::xgboost(
          params = params_best,
          data = x_dmat,
          nrounds = nrounds_best,
          sample_weight = data[[cols_lst$col_wt]],
          early_stopping_rounds = early_stopping_rounds,
          print_every_n = print_every_n,
          verbose = 1
        )
    }
    
    fit <- 
      .get_x(
        f = .f_fit, 
        path = path_fit, 
        f_import = xgboost::xgb.load,
        f_export = xgboost::xgb.save,
        append = FALSE,
        export = TRUE,
        overwrite = .overwrite$fit
      )
    
    .f_predict <- function() {
      
      preds <-
        fit %>%
        predict(x_mat) %>%
        .augment_preds(
          data = data,
          cols_id = cols_lst$cols_id,
          cols_extra = cols_lst$cols_extra,
          col_y = cols_lst$col_y,
          f_trans = .inverse_log
        )
      preds
    }
    
    preds <- 
      .get_x(
        f = .f_predict,
        path = path_preds,
        f_import = arrow::read_parquet,
        f_export = arrow::write_parquet,
        append = FALSE,
        export = TRUE,
        overwrite = .overwrite$preds
      )
    
    .f_shap_wide <- function() {
      .shap_xgb(
        data = data,
        cols_lst = cols_lst,
        x_mat = x_mat,
        fit = fit,
        preds = preds
      )
    }
    
    shap_wide <-
      .get_x(
        f = .f_shap_wide,
        path = path_shap_wide,
        f_import = arrow::read_parquet,
        f_export = arrow::write_parquet,
        append = FALSE,
        export = TRUE,
        overwrite = .overwrite$shap
      )
    fit
  }
