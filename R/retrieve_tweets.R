

#' Get Twitter token
#' 
#' Get Twitter token
#' @param ... Passed to `rtweet::create_token()`
#' @details Environment variables named "TWITTER_APP|CONSUMER_API_KEY..." are expected to exist.
#' @export
get_twitter_token <- function(...) {
  rtweet::create_token(
    app =             Sys.getenv('TWITTER_APP'),
    consumer_key =    Sys.getenv('TWITTER_CONSUMER_API_KEY'),
    consumer_secret = Sys.getenv('TWITTER_CONSUMER_API_SECRET'),
    access_token =    Sys.getenv('TWITTER_ACCESS_TOKEN'),
    access_secret =   Sys.getenv('TWITTER_ACCESS_TOKEN_SECRET'),
    ...
  )
}

#' @noRd
.get_valid_tweet_methods <- function() {
  c('all', 'none', 'since', 'new')
}

#' @noRd
.validate_tweet_method <- function(x = .get_valid_tweet_methods(), ...) {
  match.arg(x, ...)
}

#' @noRd
.distinctify_tweets <- function(...) {
  list(...) %>% 
    purrr::reduce(dplyr::bind_rows) %>% 
    dplyr::distinct(status_id, .keep_all = TRUE) %>% 
    dplyr::arrange(created_at)
}

#' Import tweets
#' 
#' Import tweets from xGPhilosophy
#' @param method How to retrieve tweets.
#' @param ... Extra parameters passed to `rtweet::get_timeline()`
#' @param tweets If not `NULL`, then this assumed to have been retrieved by the user beforehand (e.g. by calling `retrieve_tweets(export = FALSEE)`). New tweets will not be scraped. Rather, they will simply be appended to existing tweets (assuming `append = TRUE`).
#' @param user User for whom to retrieve tweets for. (xGPhilophy by default.)
#' @param n Number of tweets to retrieve. (3200 by default.)
#' @section Valid `method`s:
#' \itemize{
#'   \item `"none"` return existing tweets at `path`
#'   \item `"all"` re-trieve and return entire timeline
#'   \item `"since"` retrieve timeline since last tweet saved at `path` and return it along with existing tweets (functionally this will return the same results as `"all"` , unless a tweet has since been deleted)
#'   \item `"new"` only retrieve and return timeline since last tweet saved at `path`
#' }
#' @inheritParams do_get
#' @export
#' @rdname retrieve_tweets
retrieve_tweets <-
  function(method = .get_valid_tweet_methods(),
           user = .get_user(),
           ...,
           tweets = NULL,
           n = 3200,
           dir = .get_dir_data(),
           file = sprintf('%s_timeline', user),
           ext = 'rds',
           path = NULL,
           f_import = readr::read_rds,
           f_export = readr::write_rds,
           export = TRUE) {
    path <- .generate_path(path = path, dir = dir, file = file, ext = ext)
    path_exists <- path %>% file.exists()
    tweets_are_provided <- !is.null(tweets)
    if(method == 'none') {
      if(path_exists) {
        .display_info('Importing from `path = "{path}"`.')
        tweets_existing <- f_import(path)
        if(tweets_are_provided) {
          tweets <- .distinctify_tweets(tweets, tweets_existing)
        } else {
          tweets <- tweets_existing
        }
        # return(tweets)
      } else {
        .display_warning('`path = "{path}"` does not exist, so `method = "none"` does not make sense. Setting `method = "all"`.')
        method <- 'all'
      }
    } else if(method != 'all') {
      if(!path_exists) {
        method <- 'all'
      } else {
        .display_info('Importing from `path = "{path}"`.')
        tweets_existing <- f_import(path)
        # browser()
        n_existing <- nrow(tweets_existing)
        is_null <- is.null(tweets_existing)
        is_nrow_0 <- (n_existing == 0L)
        is_bad <- is_null || is_nrow_0
        if(is_bad) {
          if(tweets_are_provided) {
            .display_warning('Couldn\t combine `tweets` passed in with any pre-saved tweets at `path = "{path}"`.')
          } else if(is_null) {
            .display_warning('`NULL` found when attempting to import. Changing `method` to "all" and deleting existing file at `path = "{path}"`..')
            file.remove(path)
          } else if(is_nrow_0) {
            .display_warning('0 rows found when attempting to import. Changing `method` to "all".')
          }
          method <- 'all'
        } else {
          if(tweets_are_provided) {
            tweets <- .distinctify_tweets(tweets, tweets_existing)
          } else {
            now <- lubridate::now()
            n_hour_fresh <- .get_n_hour_fresh()
            # browser()
            # Refresh tweets that have been saved before but are not more than `n_hour_fresh` days old. This is what `is_fresh` marks in the predictions.
            tweets_existing <- 
              tweets_existing %>% 
              dplyr::filter(created_at <= (!!now - lubridate::hours(n_hour_fresh)))

            n_existing <- nrow(tweets_existing)
            if(n_existing == 0L) {
              method <- 'all'
            } else {
              latest_tweet <- tweets_existing %>% dplyr::slice_max(created_at)
              tweets_new <- rtweet::get_timeline(user = user, n = n, since_id = latest_tweet$status_id, ...)
              n_new <- nrow(tweets_new)
              # n_diff <- n_existing - n_new
              if(n_new > 0L) {
                .display_info('Identified {n_new} new tweets/tweets to update since they were made {n_hour_fresh} hour{ifelse(n_hour_fresh > 1L, "s", "")} ago.')
                if(method == 'since') {
                  tweets <- .distinctify_tweets(tweets_new, tweets_existing)
                } else if (method == 'new') {
                  tweets <- tweets_new
                }
              }
            }
          }
        }
      }
    }

    if(method == 'all') {
      tweets <- rtweet::get_timeline(user = user, n = n, ...)
    }
    
    if(!export) {
      return(tweets)
    }
    
    dir <- dirname(path)
    if(!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
    f_export(tweets, path)
    .display_info('Exported to `path = "{path}"`.')
    tweets
    
  }
