
#' Make predictions
#' 
#' Make predictions for new tweets, appending to existing predictions and SHAP values.
#' @inheritParams do_fit
#' @param fit xgboost fit
#' @param path_fit If fit is not provided, then the fit is imported from `path_fit`.
#' @param .path Specific paths to export the predictions and SHAP values to. If left as `NULL`, they are saved as `"preds_{stem}.rds"` and `"shap_{stem}.rds"` in the directory at `getOption("xengagement.dir_data")`. As with `.overwrite`, only use this if you know what you're doing.
#' @param ... Unused
#' @export
do_predict <- 
  function(tweets_transformed, 
           stem = get_valid_stems(), 
           overwrite = TRUE, 
           fit = NULL,
           path_fit = NULL,
           ...,
           .overwrite = list(
             preds = NULL,
             shap = NULL
           ),
           .path = list(
             preds = NULL,
             shap = NULL
           )) {
    
    .validate_stem(stem)
    has_fit <- !is.null(fit)
    has_fit_path <- !is.null(path_fit)

    .path_data_x <- function(file, ext = NULL) {
      .path_data(file = sprintf('%s_%s', file, stem), ext = ext)
    }
    
    .path_data_rds_x <- purrr::partial(.path_data_x, ext = 'rds', ... = )
    if(!has_fit & !has_fit_path) {
      
      path_fit <- .path_data_x('fit')
      if(!file.exists(path_fit)) {
        .display_error(
          'Please specify `fit` or provide a valid `path_fit.`'
        )
      }
      .display_info('Successfully imported fit from `"{path_fit}"` (since it was not provided in `fit` and `path_fit` was originally `NULL`).')
    }
    
    path_preds <- .path$preds %||% .path_data_rds_x('preds')
    path_shap <- .path$preds %||% .path_data_rds_x('shap')
    
    
    cols_lst <- .get_cols_lst(stem = stem)
    
    .overwrite$preds <- .overwrite$preds %||% overwrite
    .overwrite$shap <- .overwrite$shap %||% overwrite
    
    data <- tweets_transformed
    
    if(!has_fit) {
      .import_safely <- purrr::safely(xgboost::xgb.load, otherwise = NULL)
      fit <- .import_safely(path_fit)
      if(is.null(fit)) {
        .display_error('The `fit` loaded from `path_fit = "{path_fit}"` is not an `{xgboost}` model.')
      }
    }
    cls_fit <- class(fit)
    is_xgb <- 'xgb.Booster' %in% cls_fit
    if(!is_xgb) {
      .display_error(
        '`fit` must be of class `xgb.Booster`, which is not in {cls_fit}.'
      )
    }
    
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
        f_import = readr::read_rds,
        f_export = readr::write_rds,
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
          col_y = stringr::str_remove(cols_lst$col_y, '_log')
        )
    }
    
    shap <-
      do_get(
        f = .f_shap,
        path = path_shap,
        f_import = readr::read_rds,
        f_export = readr::write_rds,
        append = FALSE,
        export = TRUE,
        overwrite = .overwrite$shap
      )
    list(preds = preds, shap = shap)
  }
