
#' @noRd
.check_before_tweeting <- function(status_id, tweets = NULL, in_reply_to_tweets = NULL, ..., user = .get_user_bot(), in_reply_to_user = .get_user()) {
  
  # tweets_are_provided <- !is.null(tweets)
  # if(!tweets_are_provided) {
  #   .display_info('It\'s recommended to provide `tweets` explicitly. Trying to import anyways...')
  #   tweets <- 
  #     retrieve_tweets(
  #       user = user,
  #       export = FALSE, 
  #       method = 'none', 
  #       ...
  #     )
  # }
  # 
  # tweets_are_provided <- !is.null(in_reply_to_tweets)
  # if(!tweets_are_provided) {
  #   .display_info('It\'s recommended to provide `in_reply_to_tweets` explicitly. Trying to import anyways...')
  #   in_reply_to_tweets <- 
  #     retrieve_tweets(
  #       user = in_reply_to_user,
  #       export = FALSE, 
  #       method = 'none', 
  #       ...
  #     )
  # }
  
  now <- lubridate::now()
  n_minute_lookback <- .get_n_minute_lookback()
  # n_minute_lookback <- 48 * 60
  # Check that xGPhilosophy made the tweet recently
  in_reply_to_tweets_filt <-
    in_reply_to_tweets %>% 
    dplyr::filter(created_at >= (!!now - lubridate::minutes(n_minute_lookback)))
  n_row <- nrow(in_reply_to_tweets_filt)
  suffix <- sprintf('%s minute%s', n_minute_lookback, ifelse(n_minute_lookback > 1L, 's', ''))
  if(n_row == 0L) {
    in_reply_to_tweets_filt <-
      in_reply_to_tweets %>% 
      dplyr::filter(status_id == !!status_id)
    suffix2 <- ''
    if(!is.null(in_reply_to_tweets_filt) & nrow(in_reply_to_tweets_filt) == 1L) {
      created_at <- in_reply_to_tweets_filt$created_at
      diff <- {now - created_at} %>% as.numeric('hours') %>% round(1)
      suffix2 <- glue::glue('. (It was made {diff} hours ago.)')
    }
    .display_info('Tweet will not be made (for `status_id = "{status_id}"`) since the corresponding tweet was made beyond {suffix} ago{suffix2}.')
    return(FALSE)
  }

  # # NOTE: Could do something here where the bot is limited to a certain number of tweets per minute.  
  # tweets_filt <-
  #   tweets %>% 
  #   dplyr::filter(created_at >= (!!now - lubridate::minutes(1)))
  # n_row <- nrow(tweets_filt)
  # if(n_row == 0L) {
  #   .display_info('Tweet will not be made since it is beyond {suffix}.')
  #   return(FALSE)
  # }
  tweets_filt <- tweets
  
  # Check that the tweet actually matches the `text`. (I don't know when this would fail, but let's test it anyways.)
  in_reply_to_tweets_filt <-
    in_reply_to_tweets_filt %>% 
    dplyr::filter(status_id == !!status_id)
  is_null <- is.null(in_reply_to_tweets_filt)
  n_row <- nrow(in_reply_to_tweets_filt)
  suffix <- glue::glue('matching `status_id = "{status_id}"` in the past {suffix}.')
  if(is_null | n_row == 0L) {
    .display_info('No tweets from `in_reply_to_user = "{in_reply_to_user}"` {suffix}')
    return(FALSE)
  }
  
  # Check that the bot hasn't made a reply tweet already.
  tweets_filt <-
    tweets_filt %>% 
    dplyr::filter(reply_to_status_id == !!status_id)
  is_null <- is.null(tweets_filt)
  n_row <- nrow(tweets_filt)
  if(!is_null & n_row > 0L) {
    .display_info('`user = "{user}"` already tweeted something {suffix}')
    return(FALSE)
  }
  
  return(TRUE)
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
#' @param pred Data frame with `{stem}_pred`, `{stem}_pred_prnk`, `(team|g|xg)_(h|a)`, and `created_at` columns.
#' @param in_reply_to_tweets Tweets of the user for whom a tweets will be made in reply to (xGPhilosophy by default). Needed for identifying the tweet to reply to.
#' @param tweets Tweets of the user (punditratio by default) who will be making a reply. Needed to check that a tweet hasn't already been made.
#' @param preds_long Tidy predictions with at least five columns `status_id`, `stem` (either favorite or retweet), `pred`, and `count`. This will be used to generate a plot to accompany the tweet.
#' @param ... Extra parameters passed to .`save_plot()`.
#' @param user Who is making the reply. (pundit_ratio by default.)
#' @param dry_run Whether or not to actually make a tweet.
#' @export
#' @rdname generate_tweet
generate_tweet <-
  function(pred,
           tweets,
           in_reply_to_tweets,
           preds_long,
           ...,
           prelude = NULL,
           user = .get_user_bot(),
           dry_run = TRUE,
           delete_plot = !dry_run) {
    # TODO: Just use `status_id` here?
    should_tweet <-
      .check_before_tweeting(
        # text = pred$text,
        status_id = pred$status_id,
        tweets = tweets,
        in_reply_to_tweets = in_reply_to_tweets
      )
    suffix <- glue::glue('on behalf of `user = "{user}"`')
    if (!should_tweet) {
      .display_info('Not making a tweet {suffix}.')
      return(NULL)
    }
    
    if(!is.null(prelude)) {
      prelude <- glue::glue('{prelude}
      
      ')
    }
    
    text <- glue::glue('
    {prelude}xFavorites: {.f_number(pred$favorite_pred)} ({.f_percentile(pred$favorite_pred_prnk)} percentile)
    xRetweets: {.f_number(pred$retweet_pred)} ({.f_percentile(pred$retweet_pred_prnk)} percentile)
    ')
    if(dry_run) {
      .display_info('Would have made the following tweet {suffix} if not for `dry_run = TRUE`: 
                    {text}')
      return(NULL)
    }
    path_png <- .plot_actual_v_pred(preds_long = preds_long, status_id = pred$status_id, ...)
    
    if(delete_plot) {
      on.exit(file.remove(path_png), add = FALSE)
    }

    rtweet::post_tweet(
      status = text,
      in_reply_to_status_id = pred$status_id,
      media = path_png
    )
  }

# # TODO: Follow up 24 hours after the original tweet
# generate_followup_tweet <-
#   function(pred,
#            tweets,
#            in_reply_to_tweets = tweets,
#            preds_long,
#            ...,
#            user = .get_user_bot(),
#            dry_run =  TRUE) {
#     # TODO: Just use `status_id` here?
#     should_tweet <-
#       .check_before_tweeting(
#         # text = pred$text,
#         status_id = pred$status_id,
#         tweets = tweets,
#         in_reply_to_tweets = in_reply_to_tweets
#       )
#     suffix <- glue::glue('on behalf of `user = "{user}"`')
#     if (!should_tweet) {
#       .display_info('Not making a tweet {suffix}.')
#       return(NULL)
#     }
#     
#     text <- glue::glue('
#     After {} hours since original tweet:
#     # of Favorites: {.f_number(pred$favorite_count)} ({} {} than xFavorites)
#     # of Retweets: {.f_number(pred$retweet_count)} ({} {} than xRetweets)
#     
#     \U0001f517: Check my bio for more @xGPhilosophy xEngagement.
#     ')
#     if(dry_run) {
#       .display_info('Would have made the following tweet {suffix} if not for `dry_run = TRUE`: 
#                     {text}')
#       return(NULL)
#     }
#     path_png <- .plot_actual_v_pred(preds_long = preds_long, status_id = pred$status_id, ...)
#     on.exit(file.remove(path_png), add = FALSE)
#     
#     rtweet::post_tweet(
#       status = text,
#       in_reply_to_status_id = pred$status_id,
#       media = path_png
#     )
#   }
