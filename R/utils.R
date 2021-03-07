
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
.get_seed <- function() {
  getOption('xengagement.seed')
}

#' @noRd
.get_verbose <- function() {
  getOption('xengagement.verbose')
}

#' @noRd
.get_n_hour_fresh <- function() {
  getOption('xengagement.n_hour_fresh')
}

#' @noRd
.get_n_minute_delay <- function() {
  getOption('xengagement.n_minute_delay')
}

#' @noRd
.get_n_minute_lookback <- function() {
  getOption('xengagement.n_minute_lookback')
}

#' @noRd
.get_user <- function() {
  'xGPhilosophy'
}

#' @noRd
.get_user_bot <- function() {
  'punditratio'
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

#' #' @noRd
#' .is_github_aciton <- function(...) {
#'   res <- Sys.getenv('IS_GITHUB_ACTION')
#'   if(is.null(res)) {
#'     return(FALSE)
#'   }
#'   res
#' }

