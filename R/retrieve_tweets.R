
#' @inheritParams do_get
#' @rdname retrieve_tweets
.retrieve_tweets <-
  function(user = 'xGPhilosophy',
           n = 3200,
           method,
           ...,
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
    
    if(path_exists & append) {
      if(!export) {
        .display_warning('Setting `export = TRUE` since `append = TRUE` take higher priority.')
        export <- TRUE
      }
      .display_info('Importing from `path = "{path}"` for appending.')
      tweets_existing <- f_import(path)
      n_existing <- nrow(tweets_existing)
      latest_tweet <- tweets_existing %>% dplyr::slice_max(created_at)
      # tweets <- rtweet::get_timeline(user = user, n = n, max_id = latest_tweet$status_id, ...)
      tweets_new <- rtweet::get_timeline(user = user, n = n, since_id = latest_tweet$status_id)
      # `tweets_new` always shares at least 1 tweet, so need to distinct before checking number of new.
      n_new <- nrow(tweets_new)
      if(n_new > 0) {
        .display_info('Identified {n_new - n_existing} new tweets.')
        if(method == 'new') {
          tweets <- tweets_new
        } else {
          tweets <- 
            dplyr::bind_rows(tweets_new, tweets_existing) %>% 
            # dplyr::distinct(status_id, .keep_all = TRUE) %>% 
            dplyr::arrange(created_at)
        }
      } else {
        .display_info('No new tweets identified.')
        return(tweets_existing)
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
#' @details This is a wrapper around the non-exported `.retrieve_tweets()`. `append`, `export`, and `overwrite` are determined for you based on `method`.
#' @param method How to retrieve tweets.
#' @param user User for whom to retrieve tweets for.
#' @param n Number of tweets to retrieve
#' @param ... Extra parameters passed to `rtweet::get_timeline()`
#' @section Valid methods:
#' \itemize{
#'   \item `"none"` return existing tweets at `path`
#'   \item `"all"` re-trieve and return entire timeline
#'   \item `"since"` retrieve timeline since last tweet saved at `path` and return it along with existing tweets (functionally this will return the same results as `"all"` , unless a tweet has since been deleted)
#'   \item `"new"` only retrieve and return timeline since last tweet saved at `path`
#' }
#' @export
#' @rdname retrieve_tweets
retrieve_tweets <- function(method = c('none', 'all', 'since', 'new'), ...) {
  method <- match.arg(method)
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
  .retrieve_tweets(
    append = append,
    export = export,
    overwrite = overwrite,
    method = method,
    ...
  )
}
