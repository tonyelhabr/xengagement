
#' Screenshot xGPhilosophy's latest tweet
#' 
#' Screenshot xGPhilosophy's latest tweet
#' @param tweets This can be tweets retrieved from something like `retrieve_tweets()` or just a data frame with at least `created_at` and `status_id`.
#' @param ... Extra parameters passed to `tweetrmd::tweet_screenshot()`
#' @inheritParams retrieve_tweets
#' @inheritParams do_get
#' @export
#' @rdname retrieve_tweets
screenshot_latest_tweet <- 
  function(tweets = NULL,
           status_id = NULL,
           user = .get_user(), 
           dir = .get_dir_data(),
           file = sprintf('%s_latest_tweet', user),
           ext = 'png',
           path = NULL,
           ...) {
    status_id_is_provided <- !is.null(status_id)
    if(!status_id_is_provided) {
      tweets_are_provided <- !is.null(tweets)
      if(!tweets_are_provided) {
        .display_info('It\'s recommended to provide tweets explicitly. Trying to import anyways...')
        tweets <- retrieve_tweets(user = user, export = FALSE, method = 'none', ...)
      }
      latest_tweet <- tweets %>% dplyr::slice_max(created_at)
      status_id <- latest_tweet$status_id
    }
    path <- .generate_path(path = path, dir = dir, file = file, ext = ext)
    path_exists <- path %>% file.exists()
    suppressMessages(tweetrmd::tweet_screenshot(
      tweetrmd::tweet_url(user, status_id),
      file = path,
      ...
    ))
  }
