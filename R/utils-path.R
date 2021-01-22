
#' @noRd
.generate_path <- function(path = NULL, dir, file, ext) {
  if(!is.null(path)) {
    return(path)
  }
  file.path(dir, sprintf('%s.%s', file, ext))
}

#' @noRd
.path_x <- function(dir, file = tempfile(), ext = NULL) {
  if(!is.null(ext)) {
    ext <- sprintf('.%s', ext)
  } else {
    ext <- ''
  }
  file.path(dir, sprintf('%s%s', file, ext))
}

#' @noRd
.path_data <- function(dir = get_dir_data(), ...) {
  .path_x(dir = dir, ...)
}
