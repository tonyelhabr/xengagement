
#' @noRd
.toupper1 <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#' @noRd
.theme <- function(base_size = 14, ...) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      ...,
      plot.title.position = 'plot',
      title = ggplot2::element_text(size = base_size, color = 'gray20'),
      plot.title = ggplot2::element_text(face = 'bold', size = base_size, color = 'gray20'),
      axis.text = ggplot2::element_text(size = base_size),
      axis.title = ggplot2::element_text(size = base_size, face = 'bold', hjust = 0.99),
      axis.line = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = 'gray80'),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = base_size, color = 'gray20', hjust = 0),
      plot.caption = ggplot2::element_text(size = 9, color = 'gray20', hjust = 0),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}

# Put `path` first to match how most packages arrange their arguments. Note that this is used in `f_export` for `.plot_actual_v_pred()`.
#' @noRd
.save_plot <-
  function(object,
           path = NULL,
           dir = .get_dir_data(),
           file = deparse(substitute(object)),
           ext = 'png',
           height = 4,
           width = 8,
           ...) {
    path <- .generate_path(path = path, dir = dir, file = file, ext = ext)
    ggplot2::ggsave(
      plot = object,
      filename = path,
      width = width,
      height = height,
      # type = 'cairo',
      ...
    )
  }

# Note that `path` is returned. This is convenient for immediately deleting the plot after the tweet is made.
#' @noRd
.plot_actual_v_pred <- 
  function(preds_long, 
           status_id,
           ...,
           dir = .get_dir_data(),
           file = sprintf('%s_pred', status_id),
           ext = 'png',
           path = NULL,
           # f_import = readr::read_rds,
           f_export = .save_plot,
           export = TRUE) {
    path <- .generate_path(path = path, dir = dir, file = file, ext = ext)
    path_exists <- path %>% file.exists()
    
    preds_long_filt <-
      preds_long %>% 
      dplyr::filter(status_id == !!status_id)
    n_row <- nrow(preds_long_filt)
    if(n_row != 2L) {
      .display_error('Filtered data should have 2 rows, not {n_row} rows.')
    }
    viz <-
      preds_long %>% 
      ggplot2::ggplot() +
      ggplot2::aes(x = pred, y = count) +
      ggplot2::geom_point(
        ggplot2::aes(color = stem), alpha = 0.3, show.legend = FALSE
      ) +
      ggplot2::geom_point(
        data = preds_long_filt %>% dplyr::mutate(count = pred), 
        color = 'black', 
        size = 4
      ) +
      ggplot2::geom_label(
        data = preds_long_filt %>%
          dplyr::rowwise() %>% 
          dplyr::mutate(x = max(pred * 1.1, pred + 100), y = x),
        ggplot2::aes(x = x, y = y, label = glue::glue('x{stem}: {scales::number(pred, accuracy = 1, big.mark = ",")}')),
        hjust = 0,
        vjust = 0,
        size = 5
      ) +
      # ggplot2::geom_abline(ggplot2::aes(slope = 1, intercept = 0), size = 1, linetype = 2) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::scale_x_continuous(labels = scales::comma) +
      ggplot2::scale_color_manual(values = c('Favorites' = '#003f5c', 'Retweets' = '#ffa600')) +
      ggplot2::facet_wrap(~stem, scales = 'free') +
      .theme() +
      ggplot2::labs(
        title = 'xGPhilosophy Tweet Engagement',
        x = 'Predicted', y = 'Actual',
        caption = '"Actual" shown as equal to predicted for illustrative purposes. Insufficient time has passed to record the actual numbers.'
      )
    f_export(viz, path)
    path
  }

.reorder_within <- function (x, by, within, fun = mean, sep = '___', ...) {
  if (!is.list(within)) {
    within <- list(within)
  }
  new_x <- do.call(paste, c(list(x, sep = sep), within))
  stats::reorder(new_x, by, FUN = fun)
}

.scale_y_reordered <- function (..., sep = '___') {
    reg <- paste0(sep, '.+$')
    ggplot2::scale_y_discrete(labels = function(x) gsub(reg, '', x), ...)
  }

.plot_shap <- 
  function(preds_long, 
           status_id,
           ...,
           dir = .get_dir_data(),
           file = sprintf('%s_shap', status_id),
           ext = 'png',
           path = NULL,
           # f_import = readr::read_rds,
           f_export = .save_plot,
           export = TRUE) {
    path <- .generate_path(path = path, dir = dir, file = file, ext = ext)
    path_exists <- path %>% file.exists()
    
    shap_long_filt <-
      shap_long %>% 
      dplyr::filter(status_id == !!status_id)
    n_row <- nrow(shap_long_filt)
    if(n_row == 0L) {
      .display_error('Filtered data should have >0 rows.')
    }
    
    n_feature <- 5L
    shap_long_filt <-
      shap_long_filt %>% 
      dplyr::mutate(sign = ifelse(shap_value > 0, 'pos', 'neg')) %>% 
      dplyr::group_by(stem) %>% 
      dplyr::mutate(rnk = dplyr::row_number(dplyr::desc(abs(shap_value)))) %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(rnk <= !!n_feature)
    
    viz <-
      shap_long_filt %>% 
      ggplot2::ggplot() +
      ggplot2::aes(y = .reorder_within(lab, shap_value, stem), x = shap_value) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = 0)) +
      ggplot2::geom_col(ggplot2::aes(fill = sign), show.legend = FALSE) +
      .scale_y_reordered() +
      ggplot2::scale_fill_manual(values = c('neg' = '#7a5193', 'pos' = '#ef5675')) +
      .theme(base_size = 12) +
      ggplot2::theme(
        plot.title.position = 'plot',
        axis.text.x = ggplot2::element_blank(),
        # axis.title.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank()
      ) +
      ggplot2::facet_wrap(~stem, scales = 'free') +
      ggplot2::labs(
        title = sprintf('%s Most Important Factors for xEngagement', n_feature),
        y = NULL,
        x = 'Importance Relative to Average Prediction'
      )
    f_export(viz, path)
    path
  }
