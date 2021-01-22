
train <- TRUE
dir_data <- get_dir_data()
valid_stems <- get_valid_stems()

.f_transform <- function() {
  tweets <- retrieve_tweets(method = 'none')
  tweets %>% transform_tweets(train = train)
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
    .overwrite = list(tune = FALSE, fit = TRUE)
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
