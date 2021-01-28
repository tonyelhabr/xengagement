
#' @noRd
.check_before_tweeting <- function(rgx, tweets = NULL, ...) {
  
  tweets_are_provided <- !is.null(tweets)
  if(!tweets_are_provided) {
    .display_info('It\'s recommended to provide tweets explicitly. Trying to import anyways...')
    tweets <- retrieve_tweets(export = FALSE, method = 'none', ...)
  }
  now <- lubridate::now()
  n_minute_lookback <- .get_n_minute_lookback()
  # n_minute_lookback <- 48 * 60
  tweets_filt <-
    tweets %>% 
    dplyr::filter(created_at >= (!!now - lubridate::minutes(n_minute_lookback)))
  n_row <- nrow(tweets_filt)
  suffix <- sprintf('%s minute%s', n_minute_lookback, ifelse(n_minute_lookback > 1L, 's', ''))
  if(n_row == 0L) {
    suffix <- 
    .display_info('Tweet will not be made since it is beyond {suffix}.')
    return(FALSE)
  } else {
    tweets_filt <-
      tweets_filt %>% 
      dplyr::filter(stringr::str_detect(text, rgx, negate = TRUE))
    n_row <- nrow(tweets)
    if(n_row == 0L) {
      .display_info('Already tweeted somthing matching `rgx = "{rgx}"` in the past {suffix}.')
      return(FALSE)
    }
    TRUE
  }
}

#' @noRd
.f_number <- function(x) {
  # format(round(x, 0), big.mark = ',')
  scales::number(x, accuracy = 1, big.mark = ',')
}

#' @noRd
.f_percentile <- function(x) {
  # sprintf('%s%%', round(100 * x, 0))
  scales::ordinal(round(100 * x, 0))
}

#' Generate a tweet
#' 
#' Generate a tweet
#' @param pred Data frame with `{stem}_pred`, `{stem}_pred_prnk`, `(tm|g|xg)_(h|a)`, and `created_at` columns.
#' @param ... Extra parameters passed to `rtweet::post_tweet()`
#' @param user for whom to retrieve tweets for. (pundit_ratio by default.)
#' @param dry_run Whether or not to actually make a tweet.
#' @export
#' @rdname retrieve_tweets
generate_tweet <-
  function(pred, tweets, ..., user = .get_user_bot(), dry_run =  TRUE) {
    rgx <- sprintf('%s.*%s.*', pred$tm_h, pred$tm_a)
    should_tweet <- .check_before_tweeting(rgx = rgx, tweets = tweets)
    suffix <- glue::glue('on behalf of `user = "{user}"`')
    if(!should_tweet) {
      .display_info('Not making a tweet {suffix}.')
      return(NULL)
    }

    text <- glue::glue('
    {pred$tm_h} ({pred$xg_h}) {pred$g_h}-{pred$g_a} ({pred$xg_a}) {pred$tm_a}
    
    xFavorites: {.f_number(pred$favorite_pred)} ({.f_percentile(pred$favorite_pred_prnk)} percentile)
    xRetweets: {.f_number(pred$retweet_pred)} ({.f_percentile(pred$retweet_pred_prnk)} percentile)
    
    \U0001f517: Check my bio for more xGPhilosophy xEngagement.
    ')
    if(dry_run) {
      .display_info('Would have made the following tweet {suffix} if not for `dry_run = TRUE`: 
                    {text}')
      return(NULL)
    }
    rtweet::post_tweet(
      status = text,
      ...
    )
  }
