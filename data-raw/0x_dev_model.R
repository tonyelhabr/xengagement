
library(tidyverse)
library(tidymodels)

tweets_trimmed <-
  tweets %>% select(matches('^(favorite|retweet)_count$'))
rec <- 
  recipe(~., data = tweets_trimmed) %>%
  step_YeoJohnson(all_numeric())
rec
debugonce(recipes:::yj_transform)
tweets_trans <-
  rec %>% 
  prep() %>% 
  bake(new_data = tweets_trimmed)
tweets_trans
tweets_trns
tweets_trimmed
