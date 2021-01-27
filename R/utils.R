
#' @noRd
.get_dir_data <- function() {
  getOption('xengagement.dir_data')
}

#' Directory for package-stored data
#' 
#' Where to retrieve package-stored data
#' @export
get_dir_data <- .get_dir_data

#' @noRd
do_getengagement_seed <- function() {
  getOption('xengagement.seed')
}

#' @noRd
.get_verbose <- function() {
  getOption('xengagement.verbose')
}

#' Valid items to predict for
#' 
#' Valid items to predict for. Either `"favorite"` or `"retweet"`.
#' @export
get_valid_stems <- function() {
  c('favorite', 'retweet')
}

#' @noRd
.validate_stem <- function(x = get_valid_stems(), ...) {
  match.arg(x, ...)
}

#' @noRd
.get_valid_suffixes <- function() {
  c('h', 'a')
}

#' @noRd
.validate_suffix <- function(x = .get_valid_suffixes(), ...) {
  match.arg(x, ...)
}

#' @noRd
`%||%` <- function (x, y) {
  if(is.null(x)) { 
    y
  } else {
    x
  }
}
