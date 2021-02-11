
token <- get_twitter_token()

train <- TRUE
method <- ifelse(train, 'all', 'since')
dir_data <- get_dir_data()
valid_stems <- get_valid_stems()

tweets <- retrieve_tweets(method = method, token = token)
tweets_transformed <- tweets %>% transform_tweets(train = train)
.display_info('Reduced {nrow(tweets)} tweets to {nrow(tweets_transformed)} transformed tweets.')
tweets_transformed

res_fit <-
  valid_stems %>% 
  setNames(., .) %>% 
  purrr::map(
    ~ do_fit(
      tweets_transformed = tweets_transformed,
      stem = .x,
      .overwrite = list(tune = train, fit = TRUE)
    )
  )
res_fit

.pluck_fit <- function(stem) {
  .validate_stem(stem)
  res_fit %>% purrr::pluck(stem) %>% purrr::pluck('fit')
}
fit_favorite <- .pluck_fit('favorite')
fit_retweet <- .pluck_fit('retweet')
usethis::use_data(fit_favorite, fit_retweet, overwrite = TRUE)

res_preds <-
  dplyr::tibble(
    stem = valid_stems,
    fit = list(xengagement::fit_favorite, xengagement::fit_retweet)
  ) %>% 
  dplyr::mutate(
    res = purrr::map2(
      stem, fit,
      ~ xengagement::do_predict(
        tweets_transformed = tweets_transformed,
        stem = ..1,
        fit = ..2,
        .overwrite = list(preds = TRUE, shap = TRUE)
      )
    )
  )

