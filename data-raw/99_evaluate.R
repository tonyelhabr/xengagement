
# NOTE: A lot of this is similar to what is in the update script. Not sure if there is a good way to avoid duplication. This script is intended for adhoc purposes, such as looking at predictions for individual teams, computing MAPE, etc.
library(tidyverse)
library(lubridate)
library(xengagement)
valid_stems <- get_valid_stems()
.path_data_rds <- partial(.path_data, ext = 'rds', ... = )
cols_lst <- get_cols_lst(valid_stems[1])
n_hour_fresh <- getOption('xengagement.n_hour_fresh')

train <- TRUE
token <- get_twitter_token()
# method <- ifelse(train, 'all', 'since')
# dir_data <- get_dir_data()
# valid_stems <- get_valid_stems()

tweets <- retrieve_tweets(method = method, token = token)
tweets_transformed <- tweets %>% transform_tweets(train = train)
tweets_transformed %>% 
  select(idx, created_at, matches('^wt_')) %>% 
  pivot_longer(matches('^wt_')) %>% 
  ggplot() + 
  aes(x = created_at, y = value, color = name) + 
  geom_point()

tweets_transformed %>% 
  select(idx, created_at, matches('^wt_'), matches('_count$')) %>% 
  pivot_longer(matches('^wt_'), names_to = 'wt_stem', values_to = 'wt') %>%
  mutate(across(wt_stem, ~str_remove(.x, 'wt_'))) %>% 
  pivot_longer(matches('(favorite|retweet)_count$'), names_to = 'count_stem', values_to = 'count') %>% 
  mutate(across(count_stem, ~str_remove(.x, '_count'))) %>% 
  filter(wt_stem == count_stem) %>% 
  select(-count_stem) %>% 
  rename(stem = wt_stem) %>% 
  ggplot() + 
  aes(x = wt, y = count, color = stem) + 
  geom_point(show.legend = FALSE) +
  facet_wrap(~stem, scales = 'free')

.f_import_preds <- function(stem) {
  path <- .path_data_rds(file = sprintf('preds_%s', stem))
  col_res_sym <- sprintf('%s_diff', stem) %>% sym()
  col_pred_sym <- sprintf('%s_pred', stem) %>% sym()
  res <- 
    path %>% 
    read_rds() %>% 
    rename_with(~sprintf('%s_pred', stem), .cols = c(.pred)) %>% 
    select(-matches('_trans$')) %>% 
    mutate(
      !!col_res_sym := !!sym(sprintf('%s_count', stem)) - !!col_pred_sym,
    ) %>% 
    mutate(
      across(
        c(!!col_pred_sym, !!col_res_sym),
        list(prnk = ~percent_rank(.x))
      )
    )
  res
}

suppressMessages(
  preds_init <-
    valid_stems %>%
    map(.f_import_preds) %>%
    reduce(full_join) %>%
    select(
      one_of(cols_lst$cols_id),
      one_of(cols_lst$cols_extra),
      matches('^(favorite|retweet)_')
    )
)
preds_init

preds_init <-
  preds_init %>% 
  mutate(
    lab_text =
      sprintf(
        '%s: %s (%.2f) %d-%d (%.2f) %s',
        date(created_at),
        # hour(created_at),
        # wday(created_at, label = TRUE),
        team_h,
        xg_h,
        g_h,
        g_a,
        xg_a,
        team_a
      ),
    lab_hover = str_remove(lab_text, '^.*[:]\\s')
  ) %>% 
  arrange(idx)

wt_favorite <- 0.5
wt_retweet <- 0.5

now <- now()

preds_filt <-
  preds_init %>%
  filter(created_at <= (!!now - hours(n_hour_fresh))) 
preds_filt

preds_filt %>% 
  summarize(
    mape_favorite = yardstick::mape_vec(favorite_count, favorite_pred),
    rmse_favorite = yardstick::rmse_vec(favorite_count, favorite_pred),
    mape_favorite2 = mean(abs((favorite_count - favorite_pred) / favorite_count), na.rm = TRUE),
  )

preds_agg <-
  preds_filt %>% 
  summarize(
    across(
      matches('^(favorite|retweet)_(count)$'),
      list(min = min, max = max)
    )
  )

scaling_factor <-
  preds_agg %>% 
  transmute(
    scaling_factor = (favorite_count_max - favorite_count_min) / (retweet_count_max - retweet_count_min)
  ) %>% 
  pull(scaling_factor)
scaling_factor

preds <-
  preds_init %>%
  mutate(
    retweet_diff_scaled = !!scaling_factor * retweet_diff
  ) %>%
  mutate(
    total_diff = !!wt_favorite * favorite_diff+!!wt_retweet * retweet_diff_scaled,
    across(
      total_diff,
      list(
        prnk = ~ percent_rank(.x),
        rnk = ~ row_number(desc(.x))
      )
    )
  ) %>% 
  select(-matches('_scaled$')) %>% 
  arrange(total_diff_rnk)
preds

.f_select <- function(suffix) {
  preds_init %>%
    select(
      idx,
      status_id,
      team = !!sym(sprintf('team_%s', suffix)),
      favorite_count,
      retweet_count,
      favorite_pred,
      retweet_pred
    ) %>%
    mutate(side = !!suffix)
}

preds_long <-
  bind_rows(.f_select('a'), .f_select('h')) %>% 
  pivot_longer(
    # -matches('^(favorite|retweet)'),
    -c(idx, status_id, team, side),
    names_to = c('stem', 'what'),
    names_pattern = '(favorite|retweet)_(count|pred)'
  ) %>%
  pivot_wider(names_from = 'what', values_from = 'value') %>% 
  mutate(across(stem, ~ sprintf('%ss', .toupper1(.x))))
preds_long

library(tidyverse)
preds_long %>% 
  group_by(idx, stem) %>% 
  slice(1) %>% 
  ungroup() %>% 
  group_by(stem) %>% 
  summarize(
    mape = yardstick::mape_vec(count + 1, pred + 1),
    rmse = yardstick::rmse_vec(count, pred),
    r2 = yardstick::rsq_vec(count, pred)
  ) %>% 
  ungroup() %>% 
  mutate(across(rmse, ~ifelse(stem == 'Retweets', .x * scaling_factor, .x)))

# teams_filt <- c('Man City', 'Liverpool', 'Chelsea', 'Man United', 'Arsenal', 'Tottenham')
teams_filt <- 'Brighton'
viz_preds <-
  preds_long %>% 
  filter(!(team %in% teams_filt)) %>% 
  ggplot() +
  aes(x = pred, y = count) +
  geom_abline(aes(slope = 1, intercept = 0), size = 1, linetype = 2) +
  geom_point(alpha = 1, color = 'grey80') +
  geom_point(
    data = preds_long %>% filter(team %in% teams_filt),
    aes(color = team),
    show.legend = FALSE,
    size = 2,
    inherit.aes = TRUE
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  facet_wrap(~stem, scales = 'free') +
  .theme() +
  # coord_equal() +
  guides(color = guide_legend('')) +
  labs(
    title = 'xGPhilosophy Tweet Engagement',
    x = 'Predicted', y = 'Actual'
  )
viz_preds

cols_x <- 
  tibble(
    lab = c(cols_lst$cols_x_names, 'Baseline'),
    feature = c(cols_lst$cols_x, 'baseline')
  )

tweets_rescaled_long <-
  tweets_transformed %>% 
  select(
    all_of(cols_lst$cols_id),
    any_of(cols_lst$cols_x)
  ) %>% 
  as.data.frame() %>% 
  mutate(
    across(
      -all_of(cols_lst$cols_id), 
      scales::rescale
    )
  ) %>% 
  gather(
    'feature',
    'value',
    -c(cols_lst$cols_id)
  ) %>% 
  as_tibble()
tweets_rescaled_long

.f_import_shap <- function(stem) {
  path <- .path_data_rds(file = sprintf('shap_%s', stem))
  shap <- path %>% read_rds()
  shap_long <-
    shap %>%
    pivot_longer(
      -c(status_id, .pred, .actual),
      names_to = 'feature',
      values_to = 'shap_value'
    ) %>%
    mutate(
      sign = 
        case_when(
          shap_value < 0 ~ 'neg', 
          shap_value > 0 ~ 'pos',
          TRUE ~ 'neutral'
        )
    )
  
  res <-
    shap_long %>% 
    full_join(
      tweets_rescaled_long, by = c(cols_lst$cols_id, 'feature')
    ) %>% 
    left_join(cols_x, by = c('feature'))
  res
}

shap <-
  valid_stems %>%
  setNames(., .) %>% 
  map_dfr(.f_import_shap, .id = 'stem') %>% 
  rename(pred = .pred, count = .actual) %>% 
  mutate(across(sign, as.character)) %>% 
  pivot_wider(
    names_from = stem,
    values_from = c(pred, count, sign, shap_value),
    names_glue = '{stem}_{.value}',
    values_fill = list(pred = 0, count = 0, sign = 'neutral', shap_value = 0)
  ) %>% 
  filter(feature != 'baseline') %>% 
  arrange(all_of(cols_lst$cols_id), feature)
shap

shap_id_cols <- c(cols_lst$cols_id, 'lab')
shap_long <-
  shap %>% 
  select(all_of(shap_id_cols), matches('_shap_value$')) %>% 
  pivot_longer(
    -all_of(shap_id_cols),
    names_to = 'stem',
    values_to = 'shap_value'
  ) %>% 
  mutate(across(stem, ~str_remove(.x, '_shap_value$'))) %>% 
  mutate(across(stem, ~ sprintf('x%ss', .toupper1(.x))))
shap_long

shap_long %>% 
  filter(status_id == max(status_id)) %>% 
  mutate(sign = ifelse(shap_value > 0, 'pos', 'neg')) %>% 
  group_by(stem) %>% 
  mutate(rnk = row_number(desc(abs(shap_value)))) %>% 
  ungroup() %>% 
  filter(rnk <= 10L) %>% 
  ggplot() +
  aes(y = tidytext::reorder_within(lab, shap_value, stem), x = shap_value) +
  geom_col(aes(fill = sign), show.legend = FALSE) +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c('neg' = '#7a5193', 'pos' = '#ef5675')) +
  .theme(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank()
  ) +
  facet_wrap(~stem, scales = 'free') +
  labs(
    title = '10 Most Important Factors for xEngagement',
    y = NULL,
    x = 'mean(|SHAP value|)'
  )

shap_agg <-
  shap_long %>% 
  group_by(lab, stem) %>% 
  summarize(across(shap_value, ~mean(abs(.x)))) %>% 
  ungroup() %>% 
  group_by(stem) %>% 
  mutate(
    across(shap_value, list(rnk = ~row_number(desc(.x))))
  ) %>% 
  ungroup() %>% 
  arrange(stem, shap_value_rnk)
shap_agg

viz_shap_agg <-
  shap_agg %>% 
  ggplot() +
  aes(y = tidytext::reorder_within(lab, shap_value, stem), x = shap_value) +
  geom_col() +
  tidytext::scale_y_reordered() +
  # .theme() +
  theme(
    panel.grid.major.y = element_blank()
  ) +
  facet_wrap(~stem, scales = 'free') +
  labs(
    y = NULL,
    x = 'mean(|SHAP value|)'
  )
viz_shap_agg
