
#' @noRd
.toupper1 <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#' @noRd
.theme <- function(...) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      ...,
      title = ggplot2::element_text(size = 14, color = 'gray20'),
      plot.title = ggplot2::element_text(face = 'bold', size = 14, color = 'gray20'),
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 14, face = 'bold', hjust = 0.99),
      axis.line = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = 'gray80'),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 14, color = 'gray20', hjust = 0),
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
    ggplot2::ggsave(
      plot = object,
      filename = path,
      width = width,
      height = height,
      type = 'cairo',
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
           file = sprintf('%s', status_id),
           ext = 'png',
           path = NULL,
           # f_import = readr::read_rds,
           f_export = .save_plot,
           export = TRUE) {
    path <- .generate_path(path = path, dir = dir, file = file, ext = ext)
    path_exists <- path %>% file.exists()
    
    preds_long <-
      preds_long %>%
      dplyr::mutate(across(stem, ~ sprintf('%ss', .toupper1(.x))))
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
      ggplot2::geom_point(ggplot2::aes(color = stem), alpha = 0.3, show.legend = FALSE) +
      ggplot2::geom_point(data = preds_long_filt, color = 'black', size = 4) +
      ggplot2::geom_label(
        data = preds_long_filt %>% dplyr::rowwise() %>% dplyr::mutate(x = max(pred * 1.1, pred + 100), y = x),
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
