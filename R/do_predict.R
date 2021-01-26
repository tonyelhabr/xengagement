
#' Make predictions
#' 
#' Make predictions for new tweets, appending to existing predictions and SHAP values.
#' @inheritParams do_fit
#' @param ... Unused
#' @export
do_predict <- 
  function(tweets_transformed, 
           stem = get_valid_stems(), 
           overwrite = TRUE, 
           ...,
           .overwrite = list(
             preds = NULL,
             shap = NULL
           )) {
    
    .validate_stem(stem)
    cols_lst <- .get_cols_lst(stem = stem)
    
    .overwrite$preds <- .overwrite$preds %||% overwrite
    .overwrite$shap <- .overwrite$shap %||% overwrite
    
    .path_data_x <- function(file, ext = NULL) {
      .path_data(file = sprintf('%s_%s', file, stem), ext = ext)
    }
    
    .path_data_parquet_x <- purrr::partial(.path_data_x, ext = 'parquet', ... = )
    path_fit <- .path_data_x('fit')

    path_preds <- .path_data_parquet_x('preds')
    path_shap <- .path_data_parquet_x('shap')
    
    data <- tweets_transformed
    
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
      do_get(
        f = .f_predict,
        path = path_preds,
        f_import = arrow::read_parquet,
        f_export = arrow::write_parquet,
        append = FALSE,
        export = TRUE,
        overwrite = .overwrite$preds
      )
    
    .f_shap <- function() {
      shap <-
        .shap_xgb(
          x_mat = x_mat,
          fit = fit,
          preds = preds,
          # Is there a non-hacky way to get around this?
          col_y = str_remove(cols_lst$col_y, '_log')
        )
    }
    
    shap <-
      do_get(
        f = .f_shap,
        path = path_shap,
        f_import = arrow::read_parquet,
        f_export = arrow::write_parquet,
        append = FALSE,
        export = TRUE,
        overwrite = .overwrite$shap
      )
    list(preds = preds, shap = shap)
  }
