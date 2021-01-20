
#' @noRd
.distinct12_at <- function(data, col, suffix , sep = '_', ...) {
  col1 <- sprintf('%s%s%s', col, sep, suffix[1])
  col2 <- sprintf('%s%s%s', col, sep, suffix[2])
  col_sym <- sym(col)
  col1_sym <- sym(col1)
  col2_sym <- sym(col2)
  dplyr::bind_rows(
    data %>% dplyr::distinct(temp = !!col1_sym),
    data %>% dplyr::distinct(temp = !!col2_sym)
  ) %>% 
    dplyr::distinct(temp) %>% 
    dplyr::arrange(temp) %>% 
    dplyr::rename(!!col_sym := temp)
}
