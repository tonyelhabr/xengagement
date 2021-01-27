
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
#' @rdname retrieve_tweets
retrieve_tweets <-
  function(method = .get_valid_tweet_methods(),
           ...,
           tweets = NULL,
           user = 'xGPhilosophy',
           n = 3200,
           dir = .get_dir_data(),
           file = sprintf('%s_timeline', user),
           ext = 'rds',
           path = NULL,
           f_import = readr::read_rds,
           f_export = readr::write_rds,
           append = TRUE,
           export = TRUE,
           overwrite = FALSE) {
    path <- .generate_path(path = path, dir = dir, file = file, ext = ext)
    path_exists <- path %>% file.exists()
    
    if(path_exists & !overwrite & !append) {
      .display_info('Importing from `path = "{path}"`.')
      return(f_import(path))
    }
    
    tweets_are_provided <- !is.null(tweets)
    if(path_exists & append | tweets_are_provided) {
      if(!export) {
        .display_warning('Setting `export = TRUE` since `append = TRUE` take higher priority.')
        export <- TRUE
      }
      .display_info('Importing from `path = "{path}"` for appending.')
      tweets_existing <- f_import(path)
      n_existing <- nrow(tweets_existing)
      
      if(tweets_are_provided) {
        tweets <- .distinctify_tweets(tweets, tweets_existing)
      } else {
        latest_tweet <- tweets_existing %>% dplyr::slice_max(created_at)
        tweets_new <- rtweet::get_timeline(user = user, n = n, since_id = latest_tweet$status_id)
        
        n_new <- nrow(tweets_new)
        if(n_new > 0) {
          .display_info('Identified {n_new - n_existing} new tweets.')
          if(method == 'new') {
            tweets <- tweets_new
          } else {
            tweets <- .distinctify_tweets(tweets_new, tweets_existing)
          }
        } else {
          .display_info('No new tweets identified.')
          return(tweets_existing)
        } 
      }
    } else {
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


#' Import tweets
#' 
#' Import tweets from xGPhilosophy
#' @details This is a wrapper around the non-exported `retrieve_tweets()`. `append`, `export`, and `overwrite` are determined for you based on `method`.
#' @export
#' @rdname retrieve_tweets
retrieve_tweets_auto <- function(method = .get_valid_tweet_methods(), ...) {
  .validate_tweet_method(method)
  if(method == 'all') {
    append <- FALSE
    export <- TRUE
    overwrite <- TRUE
  } else if(method == 'new' | method == 'since') {
    append <- TRUE
    export <- TRUE
    overwrite <- TRUE
  } else if(method == 'none') {
    append <- FALSE
    export <- FALSE
    overwrite <- FALSE
  }
  retrieve_tweets(
    append = append,
    export = export,
    overwrite = overwrite,
    method = method,
    ...
  )
}
