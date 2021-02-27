
#' @seealso \url{https://r-pkgs.org/r.html?q=onLoad#when-you-do-need-side-effects}
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.xengagement <- list(
    xengagement.dir_data = file.path('inst', 'extdata'),
    engagement.seed = 42L,
    # How many hours after xGPhilophy makes a tweet should it continue to be scraped and updated (in terms of favorites and retweets)?
    xengagement.n_hour_fresh = 1L + 24L,
    # How long (in minutes) should the bot wait for a manual tweet before auto-matically making a tweet?
    xengagement.n_minute_delay = 10,
    # What is the max time after xGPhilosophy makes a tweet that the bot can make a tweet in response?
    xengagement.n_minute_lookback = 60,
    # TODO: Make a tweet to check how accurate the initial tweet was?
    # xengagement.n_hour_followup = 18L,
    xengagement.verbose = TRUE
  )
  toset <- !(names(op.xengagement) %in% names(op))
  if(any(toset)) options(op.xengagement[toset])

  invisible()
}
