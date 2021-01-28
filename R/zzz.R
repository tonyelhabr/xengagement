
#' @seealso \url{https://r-pkgs.org/r.html?q=onLoad#when-you-do-need-side-effects}
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.xengagement <- list(
    xengagement.dir_data = file.path('inst', 'extdata'),
    engagement.seed = 42L,
    # How many hours after xGPhilophy makes a tweet should it continue to be scraped and updated (in terms of favorites and retweets)?
    xengagement.n_hour_fresh = 24L,
    # What is the max time after xGPhilosophy makes a tweet that the bot can make a tweet in response?
    xengagement.n_minute_lookback = 60L,
    xengagement.verbose = TRUE
  )
  toset <- !(names(op.xengagement) %in% names(op))
  if(any(toset)) options(op.xengagement[toset])

  invisible()
}
