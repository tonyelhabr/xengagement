
train <- FALSE
method <- ifelse(train, 'all', 'new')
dir_data <- get_dir_data()
valid_stems <- get_valid_stems()

.f_transform <- function() {
  tweets <- retrieve_tweets(method = method)
  tweets_transformed <- tweets %>% transform_tweets(train = train)
  .display_info('Reduced {nrow(tweets)} tweets to {nrow(tweets_transformed)} transformed tweets.')
  tweets_transformed
}

tweets_transformed <-
  do_get(
    f = .f_transform,
    path = file.path(dir_data, 'tweets_transformed.parquet'),
    f_import = arrow::read_parquet,
    f_export = arrow::write_parquet,
    overwrite = TRUE,
    export = TRUE
  )
tweets_transformed

purrr::walk(
  valid_stems,
  ~ do_fit(
    tweets_transformed = tweets_transformed,
    stem = .x,
    .overwrite = list(tune = train, fit = TRUE)
  )
)

purrr::walk(
  valid_stems,
  ~ do_predict(
    tweets_transformed = tweets_transformed,
    stem = .x,
    .overwrite = list(preds = TRUE, shap = TRUE)
  )
)
