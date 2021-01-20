
train <- TRUE
# tweets <- import_tweets(train = train)
tweets <- .import_tweets(overwrite = FALSE, append = FALSE, export = FALSE)
valid_stems <- get_valid_stems()

f <- if(train) {
  do_fit_model
} else {
  predict_new
}
purrr::walk(valid_stems, ~f(tweets = tweets, stem = .x))
purrr::walk(valid_stems, ~do_fit_model(tweets = tweets, stem = .x, .overwrite = list(tune = FALSE, fit = FALSE, preds = FALSE, shap = TRUE)))
