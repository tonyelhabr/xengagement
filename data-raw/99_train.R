

token <- rtweet::create_token(
  app =             Sys.getenv('TWITTER_APP'),
  consumer_key =    Sys.getenv('TWITTER_CONSUMER_API_KEY'),
  consumer_secret = Sys.getenv('TWITTER_CONSUMER_API_SECRET'),
  access_token =    Sys.getenv('TWITTER_ACCESS_TOKEN'),
  access_secret =   Sys.getenv('TWITTER_ACCESS_TOKEN_SECRET')
)

train <- FALSE
method <- ifelse(train, 'all', 'since')
dir_data <- get_dir_data()
valid_stems <- get_valid_stems()

.f_transform <- function() {
  tweets <- retrieve_tweets(method = train, token = toekn)
  tweets_transformed <- tweets %>% transform_tweets(train = train)
  .display_info('Reduced {nrow(tweets)} tweets to {nrow(tweets_transformed)} transformed tweets.')
  tweets_transformed
}

tweets_transformed <-
  do_get(
    f = .f_transform,
    path = file.path(dir_data, 'tweets_transformed.rds'),
    f_import = readr::read_rds,
    f_export = readr::write_rds,
    overwrite = TRUE,
    export = TRUE
  )
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

.pluck_fit <- function(stem) {
  .validate_stem(stem)
  res_fit %>% purrr::pluck(stem) %>% purrr::pluck('fit')
}
fit_favorite <- .pluck_fit('favorite')
fit_retweet <- .pluck_fit('retweet')
usethis::use_data(fit_favorite, fit_retweet, overwrite = TRUE)
