
#' @seealso \url{https://r-pkgs.org/r.html?q=onLoad#when-you-do-need-side-effects}
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.xengagement <- list(
    xengagement.dir_data = file.path('inst', 'extdata'),
    xengagement.seed = 42,
    xengagement.verbose = TRUE
  )
  toset <- !(names(op.xengagement) %in% names(op))
  if(any(toset)) options(op.xengagement[toset])

  invisible()
}

#' #' @seealso \url{https://github.com/hrbrmstr/hrbrthemes/blob/master/R/zzz.r}
#' .onAttach <- function(libname, pkgname) {
#'   if (.Platform$OS.type == 'windows')  { # nocov start
#'     if (interactive()) packageStartupMessage('Registering Windows fonts with R')
#'     extrafont::loadfonts('win', quiet = TRUE)
#'   }
#' }
