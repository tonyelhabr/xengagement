

#' @noRd
.check_before_tweeting <-
  function(status_id,
           tweets = NULL,
           in_reply_to_tweets = NULL,
           ...,
           user = .get_user_bot(),
           in_reply_to_user = .get_user()) {
    
    
    now <- lubridate::now(tzone = 'UTC')
    n_minute_delay <- .get_n_minute_delay()
    
    in_reply_to_tweets_filt_init <-
      in_reply_to_tweets %>% 
      dplyr::filter(status_id == !!status_id) %>% 
      dplyr::mutate(
        min_diff = as.numeric(difftime(!!now, created_at, units = 'mins'))
      )
    
    suffix2 <- ''
    if(!is.null(in_reply_to_tweets_filt_init) & nrow(in_reply_to_tweets_filt_init) == 1L) {
      created_at <- in_reply_to_tweets_filt_init$created_at
      diff <- in_reply_to_tweets_filt_init$min_diff %>% round(1)
      suffix2 <- glue::glue('. (It was made {diff} minutes ago.)')
    }
    
    # Check that xGPhilosophy did not make the tweet within the last `n_minute_delay` minutes. This allows for time for a manual tweet.
    in_reply_to_tweets_filt <-
      in_reply_to_tweets_filt_init %>% 
      dplyr::filter(min_diff >= n_minute_delay)
    n_row <- nrow(in_reply_to_tweets_filt)
    suffix <- sprintf('%s minute%s', n_minute_delay, ifelse(n_minute_delay > 1L, 's', ''))
    
    if(n_row == 0L) {
      .display_info('Tweet will not be made (for `status_id = "{status_id}"`) since the corresponding tweet was made within {suffix} ago{suffix2}.')
      return(list(should_tweet = FALSE, show_preview = TRUE))
    }
    
    n_minute_lookback <- .get_n_minute_lookback()
    # Check that xGPhilosophy make the tweet within the last `n_minute_lookback` minutes. We want to cap the lag time so that we aren't making a reply to a tweet days ago.
    in_reply_to_tweets_filt <-
      in_reply_to_tweets_filt_init %>% 
      dplyr::filter(min_diff <= n_minute_lookback)
    n_row <- nrow(in_reply_to_tweets_filt)
    
    suffix <- sprintf('%s minute%s', n_minute_lookback, ifelse(n_minute_lookback > 1L, 's', ''))
    if(n_row == 0L) {
      .display_info('Tweet will not be made (for `status_id = "{status_id}"`) since the corresponding tweet was made beyond {suffix} ago{suffix2}.')
      return(list(should_tweet = FALSE, show_preview = FALSE))
    }
    
    # # Could do something here where the bot is limited to a certain number of tweets per minute.  
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
      return(list(should_tweet = FALSE, show_preview = FALSE))
    }
    
    # Check that the bot hasn't made a reply tweet already.
    tweets_filt <-
      tweets_filt %>% 
      dplyr::filter(reply_to_status_id == !!status_id)
    is_null <- is.null(tweets_filt)
    n_row <- nrow(tweets_filt)
    if(!is_null & n_row > 0L) {
      .display_info('`user = "{user}"` already tweeted something {suffix}')
      return(list(should_tweet = FALSE, show_preview = FALSE))
    }
    
    list(should_tweet = TRUE, show_preview = TRUE)
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
#' @param preds Predictions with at least `team_h`, `team_a`, `{stem}_count`.
#' @param preds_long Tidy predictions with at least five columns `status_id`, `stem` (either favorite or retweet), `pred`, and `count`. This will be used to generate a plot to accompany the tweet.
#' @param shap_long Tidy SHAP values with at least four columns `status_id`, `stem` (either favorite or retweet), `lab`, and `shap_value`. This will be used to generate a plot to accompany the tweet.
#' @param ... Extra parameters passed to .`save_plot()`.
#' @param prelude,appendix Extra text to add to beginning or end of auto-generated tweet.
#' @param user Who is making the reply. (pundit_ratio by default.)
#' @param dry_run Whether or not to actually make a tweet.
#' @param delete_plot Whether or not to delete plots made and attached to tweet.
#' @param override Whether or not to override time filtering criteria (useful for testing).
#' @export
#' @rdname generate_tweet
generate_tweet <-
  function(pred,
           tweets,
           in_reply_to_tweets,
           preds,
           preds_long,
           shap_long,
           ...,
           prelude = NULL,
           appendix = NULL,
           user = .get_user_bot(),
           dry_run = TRUE,
           delete_plot = !dry_run,
           override = FALSE) {
    
    status_id <- pred$status_id
    res_should_tweet <-
      .check_before_tweeting(
        # text = pred$text,
        status_id = status_id,
        tweets = tweets,
        in_reply_to_tweets = in_reply_to_tweets
      )
    suffix <- glue::glue('on behalf of `user = "{user}"`')
    if (!res_should_tweet$should_tweet & !override) {
      if (!res_should_tweet$show_preview) {
        .display_info('Not making a tweet {suffix} for `status_id = "{status_id}"` ("{pred$lab_text}").')
        return(NULL)
      } else {
        # .display_info('Preview for a tweet {suffix}.')
      }
    }
    
    if(!is.null(prelude)) {
      prelude <- glue::glue('{prelude}
      
      ')
    } else {
      prelude <- ''
    }
    
    if(!is.null(appendix)) {
      appendix <- glue::glue('
      {appendix}
      ')
    } else {
      appendix <- ''
    }
    
    # TODO: Make a function for this.
    team_h <- pred$team_h
    pred_h <-
      preds %>% 
      dplyr::filter(team_h == !!team_h | team_a == !!team_h) %>% 
      dplyr::arrange(dplyr::desc(created_at)) %>% 
      dplyr::filter(status_id != !!status_id) %>% 
      dplyr::slice(1)
    h_is_home <- team_h == pred_h$team_h
    sign_h <- ifelse(h_is_home, 'vs.', '@')
    team_ha <- ifelse(h_is_home, pred_h$team_a, pred_h$team_h)
    
    team_a <- pred$team_a
    pred_a <-
      preds %>% 
      dplyr::filter(team_h == !!team_a | team_a == !!team_a) %>% 
      dplyr::arrange(dplyr::desc(created_at)) %>% 
      dplyr::filter(status_id != !!status_id) %>% 
      dplyr::slice(1)
    a_is_home <- team_a == pred_a$team_h
    sign_a <- ifelse(a_is_home, 'vs.', '@')
    team_ah <- ifelse(a_is_home, pred_a$team_a, pred_a$team_h)
    
    suffix_favorite <- 
      ifelse(
        pred$favorite_pred_prnk >= 0.75, 
        glue::glue(' ({.f_percentile(pred$favorite_pred_prnk)} percentile)'),
        ''
      )
    
    suffix_retweet <- 
      ifelse(
        pred$retweet_pred_prnk >= 0.75, 
        glue::glue(' ({.f_percentile(pred$retweet_pred_prnk)} percentile)'),
        ''
      )
    
    # Note that we must mention the account that we are replying to in order to attach media
    text <- glue::glue("
    {prelude}xEngagment for @xGPhilosophy
    xFavs: {.f_number(pred$favorite_pred)}{suffix_favorite}
    xRTs: {.f_number(pred$retweet_pred)}{suffix_retweet}
    
    Last match for {team_h} ({sign_h} {team_ha})::
    # of Favs: {.f_number(pred_h$favorite_count)}
    # of RTs: {.f_number(pred_h$retweet_count)} 
    
    Last match for {team_a} ({sign_a} {team_ah}):
    # of Favs: {.f_number(pred_a$favorite_count)}
    # of RTs: {.f_number(pred_a$retweet_count)}{appendix}
    ")
    
    path_png_preds <- .plot_actual_v_pred(preds_long = preds_long, status_id = pred$status_id, ...)
    path_png_shap <- .plot_shap(shap_long = shap_long, status_id = pred$status_id, ...)
    # if(delete_plot) {
    #   on.exit(file.remove(path_png_preds), add = FALSE)
    #   on.exit(file.remove(path_png_shap), add = FALSE)
    # }
    
    if(dry_run & !override) {
      .display_info('Would have made the following tweet {suffix} if not for `dry_run = TRUE`: 
                    {text}')
      return(NULL)
    }
    if(interactive()) {
      clipr::write_clip(text)
    }
    if(!res_should_tweet$should_tweet & !override) {
      .display_info('Would have made the following tweet {suffix} if not for a time filter not being satisfied: 
                    {text}')
      return(NULL)
    }
    # browser()
    rtweet::post_tweet(
      status = text,
      in_reply_to_status_id = pred$status_id,
      media = c(path_png_preds, path_png_shap)
      # media = path_png_shap
    )
    if(delete_plot) {
      on.exit(file.remove(path_png_preds), add = FALSE)
      on.exit(file.remove(path_png_shap), add = FALSE)
    }
    invisible()
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
#     # of Favs: {.f_number(pred$favorite_count)} ({} {} than xFavs)
#     # of RTs: {.f_number(pred$retweet_count)} ({} {} than xRTs)
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
