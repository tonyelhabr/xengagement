
#' @details You probably don't want to change any of the parameters of this function manually.
#' @describeIn import_tweets Import tweets from xGPhilosophy
#' @param user User for whom to retrieve tweets for.
#' @param n Number of tweets to retrieve
#' @param ... Extra parameters passed to `rtweet::get_timeline()`
#' @param dir Directory to use to generate `path` if `path` is not explicitly provided.
#' @param file File name (without extension) to generate `path` if `path` is not explicitly provided.
#' @param ext File extension to use to generate `path` if `path` is not explicitly provided.
#' @param path Path to export to.
#' @param f_import Function to import with if file exists and `overwrite = TRUE`.
#' @param f_export Function to export with if `export = TRUE` .
#' @param append Whether to append. Supersedes `export` and `overwrite`.
#' @param export Whether to export. Supersedes `overwrite.
#' @param overwrite Whether to overwrite.
#' @export
.import_tweets <-
  function(user = 'xGPhilosophy',
           n = 3200,
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
      latest_tweet <- tweets_existing %>% dplyr::slice_max(created_at)
      # tweets <- rtweet::get_timeline(user = user, n = n, max_id = latest_tweet$status_id, ...)
      tweets_new <- rtweet::get_timeline(user = user, n = n, since_id = latest_tweet$status_id)
      tweets <- 
        dplyr::bind_rows(tweets_new, tweets_existing) %>% 
        dplyr::distinct(status_id, .keep_all = TRUE) %>% 
        dplyr::arrange(created_at)
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
#' @details This is a wrapper around the non-exported `.import_tweets()`. `append`, `export`, and `overwrite` are determined for you based on `train`.
#' @param train Whether to re-pull all tweets (`TRUE`), or just grab the newest (`FALSE`).
#' @rdname import_tweets
import_tweets <- function(train = TRUE, ...) {
  if(train) {
    append <- FALSE
    export <- TRUE
    overwrite <- TRUE
  } else {
    append <- TRUE
    export <- TRUE
    overwrite <- TRUE
  }
  .import_tweets(
    append = append,
    export = export,
    overwrite = overwrite,
    ...
  )
}
