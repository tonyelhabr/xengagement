
# A lot of this is similar to what is in the update script. Not sure if there is a good way to avoid duplication. This script is intended for adhoc purposes, such as looking at predictions for individual teams, computing MAPE, etc.
library(tidyverse)
library(lubridate)
library(xengagement)

extrafont::loadfonts(device = 'win', quiet = TRUE)
theme_set(theme_minimal())
theme_update(
  text = element_text(family = 'Karla'),
  title = element_text('Karla', size = 14, color = 'gray20'),
  plot.title = element_text('Karla', face = 'bold', size = 18, color = 'gray20'),
  plot.title.position = 'plot',
  plot.subtitle = element_text('Karla', face = 'bold', size = 14, color = 'gray50'),
  axis.text = element_text('Karla', size = 14),
  # axis.title = element_text(size = 24, face = 'bold'),
  axis.title = element_text(size = 14, face = 'bold', hjust = 0.99),
  # axis.text = element_text('Karla', size = 12, face = 'bold', color = 'gray20'),
  # axis.title.x = element_text(hjust = 0.95),
  # axis.title.y = element_text(hjust = 0.95),
  # axis.line = element_line(color = 'gray80'),
  axis.line = element_blank(),
  panel.grid.major = element_line(color = 'gray80'),
  panel.grid.minor = element_line(color = 'gray80'),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  # plot.margin = margin(25, 25, 25, 25),
  plot.margin = margin(10, 10, 10, 10),
  # plot.background = element_rect(fill = '#F3F4F6', color = '#F3F4F6'),
  plot.background = element_rect(fill = '#ffffff', color = NA),
  # plot.caption = element_text(size = 15, face = 'italic'),
  plot.caption = element_text('Karla', size = 14, color = 'gray20', hjust = 1),
  plot.caption.position = 'plot',
  plot.tag = ggtext::element_markdown('Karla', size = 12, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.02),
  # legend.text = element_text(size = 14),
  legend.text = element_text(size = 14),
  # legend.background = element_rect(fill = '#F3F4F6', color = '#F3F4F6'),
  # legend.position = c(.85, .85))
  strip.text = element_text(size = 18),
  strip.background = element_blank(),
  panel.background = element_rect(fill = '#ffffff', color = NA)
)
update_geom_defaults('text', list(family = 'Karla', size = 4))

valid_stems <- get_valid_stems()
.path_data_rds <- partial(.path_data, ext = 'rds', ... = )
cols_lst <- get_cols_lst(valid_stems[1])
n_hour_fresh <- getOption('xengagement.n_hour_fresh')

train <- TRUE
token <- get_twitter_token()
method <- ifelse(train, 'all', 'since')
# dir_data <- get_dir_data()
# valid_stems <- get_valid_stems()

tweets <- retrieve_tweets(method = method, token = token)
tweets_transformed <- tweets %>% transform_tweets(train = train)

# favorites vs retweets
tweets_transformed %>% 
  # select(matches('_count$')) %>% 
  ggplot() + 
  aes(x = favorite_count, y = retweet_count) + 
  geom_point(aes(color = is_weekend %>% factor())) +
  # scale_color_viridis() +
  # scico::scale_color_scico_d() +
  theme(legend.position = 'top') +
  geom_smooth(method = 'lm', formula = formula(y ~ x + 0), se = FALSE)

# followers_count <- tweets %>% slice(1) %>% pull(followers_count)
# tweets_transformed %>% 
#   ggplot() +
#   aes(x = created_at, y = estimated_followers_count) +
#   geom_point() +
#   geom_point(aes(y = estimated_followers_count * (estimated_followers_count/!!followers_count)^2), color = 'red')

# wts over time
tweets_transformed %>% 
  select(idx, created_at, matches('^wt_')) %>% 
  pivot_longer(matches('^wt_')) %>% 
  ggplot() + 
  aes(x = created_at, y = value, color = name) + 
  geom_point()

# wts vs count
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

preds_init <-
  preds_init %>% 
  mutate(
    lab_text =
      sprintf(
        '%s: %s (%.2f) %d-%d (%.2f) %s',
        lubridate::date(created_at),
        team_h,
        xg_h,
        g_h,
        g_a,
        xg_a,
        team_a
      )
  ) %>% 
  arrange(idx)
wt_favorite <- 0.5
wt_retweet <- 0.5

now <- now()

preds_filt <-
  preds_init %>%
  filter(created_at <= (!!now - lubridate::hours(n_hour_fresh))) 

metrics <-
  preds_filt %>% 
  summarize(
    mape_favorite = yardstick::mape_vec(favorite_count + 1, favorite_pred + 1),
    rmse_favorite = yardstick::rmse_vec(favorite_count, favorite_pred),
    r2_favorite = yardstick::rsq_vec(favorite_count, favorite_pred),
    # mape_favorite2 = mean(abs((favorite_count - favorite_pred) / favorite_count), na.rm = TRUE),
    mape_retweet = yardstick::mape_vec(retweet_count + 1, retweet_pred + 1),
    rmse_retweet = yardstick::rmse_vec(retweet_count, retweet_pred),
    r2_retweet = yardstick::rsq_vec(retweet_count, retweet_pred)
    # mape_retweet2 = mean(abs((retweet_count - retweet_pred) / retweet_count), na.rm = TRUE)
  ) %>% 
  pivot_longer(matches('.*'), names_to = 'metric', values_to = 'valule')
metrics

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

.f_select <- function(side) {
  preds_init %>%
    select(
      idx,
      status_id,
      team = !!sym(sprintf('team_%s', side)),
      favorite_count,
      retweet_count,
      favorite_pred,
      retweet_pred
    ) %>%
    mutate(side = !!side)
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

# re-doing metrics, in a tidy-er way
metrics <-
  preds_long %>% 
  group_by(idx, stem) %>% 
  # just one of the stems, not both (don't overcount)
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
metrics

# teams_ex <- c('Man City', 'Liverpool', 'Chelsea', 'Man United', 'Arsenal', 'Tottenham')

colors <- team_mapping %>% pull(color_pri, team)
status_id_ex <- '1309847833739231233'
preds_ex <- preds %>% filter(status_id == !!status_id_ex)
preds_long_ex <- preds_long %>% filter(status_id == !!status_id_ex)
stopifnot(nrow(preds_x) == 1L)
team_ex <- preds_ex$team_h
team_ex_opp <- preds_ex$team_a
teams_ex <- c(team_ex, team_ex_opp)
lab_text_ex <- glue::glue('<b>{lubridate::date(preds_ex$created_at)}</b><br/><b><span style="color:{unname(colors[preds_ex$team_h])}">{preds_ex$team_h}</span></b>: {preds_ex$g_h} ({preds_ex$xg_h})<br/><b><span style="color:{unname(colors[preds_ex$team_a])}">{preds_ex$team_a}</span></b>: {preds_ex$g_a} ({preds_ex$xg_a})')
lab_text_ex

viz_preds_base <-
  preds_long %>% 
  ggplot() +
  aes(x = pred, y = count) +
  geom_abline(aes(slope = 1, intercept = 0), size = 1, linetype = 2) +
  # geom_point(alpha = 1, color = 'grey80') +
  geom_point(aes(color = stem), alpha = 0.2, show.legend = FALSE) +
  scale_color_manual(values = c('Favorites' = '#003f5c', 'Retweets' = '#ffa600')) +
  facet_wrap(~stem, scales = 'free') +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = '@xGPhilosophy Tweet Engagement',
    tag = '**Viz**: @TonyElHabr',
    x = 'Predicted', y = 'Actual'
  )
viz_preds_base

.f_save_preds <- partial(.save_plot, height = 7, width = 7 * 1.618, type = 'cairo', ... = )
.f_save_preds(viz_preds_base)

.f_mark <- partial(
  ggforce::geom_mark_circle,
  label.family = 'Karla',
  color = 'black',
  ... = 
)

viz_preds_ex <-
  viz_preds_base +
  ggnewscale::new_scale_color() +
  geom_point(
    data = preds_long %>% filter(team %in% teams_ex) %>% filter(status_id != status_id_ex),
    aes(color = team),
    alpha = 1,
    size = 2,
    inherit.aes = TRUE
  ) +
  scale_color_manual(values = colors) +
  geom_point(
    data = preds_long_ex %>% filter(team == !!team_ex),
    color = 'black',
    size = 5
  ) +
  .f_mark(
    data = 
      preds_ex %>% 
      mutate(stem = 'Favorites'),
    aes(x = favorite_pred, y = favorite_count, description = 'xFavorites model slightly under-forecasts number of favorites.'),
    label.buffer = unit(0.3, 'npc'),
    label.hjust = 0
  ) +
  .f_mark(
    data = 
      preds_ex %>% 
      mutate(stem = 'Retweets'),
    aes(x = retweet_pred, y = retweet_count, description = 'xRetweets model significantly under-forecasts number of retweets.'),
    # label.fontsize = 10,
    label.buffer = unit(0.5, 'npc'),
    label.hjust = 1
  ) +
  ggtext::geom_richtext(
    data = tibble(stem = 'Retweets'),
    family = 'Karla',
    label.color = NA,
    hjust = 1,
    size = 5,
    aes(x = preds_ex$retweet_pred - 100, y = preds_ex$retweet_count, label = !!lab_text_ex)
  ) +
  # guides(color = guide_legend('', override.aes = list(size = 4, alpha = 1))) +
  theme(
    plot.caption = ggtext::element_markdown(size = 12),
    legend.position = 'none'
  ) +
  labs()
viz_preds_ex
.f_save_preds(viz_preds_ex)

library(gt)
# Methods to get logos
# Look at Tom Mocks' NFL function (https://github.com/jthomasmock/espnscrapeR/blob/master/R/get_nfl_teams.R), but change format to https://a.espncdn.com/i/teamlogos/soccer/500/360.png. the EPL teams have numbers associated with them instead of abbreviations, which makes this a pain in the ass.
# Scrape from 538's table, which just scrapes from ESPN.
.f_rename <- function(side) {
  team_mapping %>% 
    select(team, logo = url_logo_espn) %>% 
    rename_all(~sprintf('%s_%s', .x, side))
}

# Reference: https://themockup.blog/posts/2020-09-26-functions-and-themes-for-gt-tables/?panelset4=theme-code3
.gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Karla"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) 
}

team_filt <- 'Brighton'
c_filt <- team_mapping %>% filter(team == !!team_filt) %>% pull(color_sec)
tb_ex <-
  preds %>% 
  mutate(
    date = lubridate::date(created_at),
    rnk = row_number(-total_diff_prnk)
  ) %>%
  left_join(.f_rename('h')) %>% 
  left_join(.f_rename('a')) %>% 
  select(
    date,
    team_h,
    logo_h,
    team_a,
    logo_a,
    g_h,
    g_a,
    xg_h,
    xg_a,
    matches('(favorite|retweet)_(count|pred)$'),
    rnk
  ) %>% 
  arrange(rnk) %>% 
  filter(rnk <= 10) %>% 
  gt::gt() %>% 
  gt::cols_label(
    .list = 
      list(
        date = 'Date',
        team_h = 'Home',
        logo_h = ' ',
        team_a = 'Away',
        logo_a = ' ',
        g_h = 'Home',
        g_a = 'Away',
        xg_h = 'Home',
        xg_a = 'Away',
        favorite_count = 'Actual',
        favorite_pred = 'Predicted',
        retweet_count = 'Actual',
        retweet_pred = 'Predicted',
        rnk = gt::md('**EOE Rank**')
      )
  ) %>%
  gt::text_transform(
    locations = gt::cells_body(
      vars(logo_h, logo_a)
    ),
    fn = function(x) {
      gt::web_image(
        url = x,
        height = 25
      )
    }
  ) %>% 
  tab_footnote(
    footnote = 'Engagement over Expected (EOE)',
    locations = cells_column_labels(columns = vars(rnk))
  ) %>% 
  tab_spanner(
    label = 'Team',
    columns = vars(team_h, logo_h, team_a, logo_a)
  ) %>% 
  tab_spanner(
    label = 'Goals',
    columns = vars(g_h, g_a)
  ) %>% 
  tab_spanner(
    label = 'xG',
    columns = vars(xg_h, xg_a)
  ) %>%
  tab_spanner(
    label = 'Favorites',
    columns = vars(favorite_count, favorite_pred)
  ) %>% 
  tab_spanner(
    label = 'Retweets',
    columns = vars(retweet_count, retweet_pred)
  ) %>% 
  fmt_number(
    columns = vars(favorite_count, retweet_count),
    decimals = 0,
    use_seps = TRUE
  ) %>% 
  fmt_number(
    columns = vars(favorite_pred, retweet_pred),
    decimals = 0,
    use_seps = TRUE
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = c_filt)
    ),
    locations = cells_body(rows = team_h == !!team_filt | team_a == !!team_filt)
  ) %>% 
  .gt_theme_538() %>% 
  tab_source_note(
    'Table theme (538 style): @thomas_mock'
  )
tb_ex
gt::gtsave(tb_ex, file.path(dir_data, 'tb_ex.png'))

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
  mutate(across(stem, ~ sprintf('%ss', .toupper1(.x))))
shap_long

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

n_col <- shap_agg %>% slice_max(shap_value_rnk, with_ties = FALSE) %>% pull(shap_value_rnk)
n_show <- n_col
n_diff <- n_col - n_show
n_diff

shap_long_ex <-
  shap_long %>% 
  # filter(status_id == max(status_id)) %>% 
  filter(status_id == !!status_id_ex) %>% 
  mutate(sign = ifelse(shap_value > 0, 'pos', 'neg')) %>% 
  group_by(stem) %>% 
  mutate(rnk = row_number(desc(abs(shap_value)))) %>% 
  ungroup() %>% 
  filter(rnk <= !!n_show)

lab_title_shap <- 'Most Important Features for @xGPhilosophy\'s xEngagement'
# lab_caption <- sprintf('%s features not shown.', n_diff)

.add_common_layers <- function(...) {
  list(
    ...,
    theme(panel.grid.major.y = element_blank()),
    labs(
      title = 'Most Important Features for @xGPhilosophy\'s xEngagement',
      y = NULL,
      tag = '**Viz**: @TonyElHabr',
      # caption = sprintf('%s features not shown.', n_diff)
      caption = ''
    )
  )
}

viz_shap_ex <-
  shap_long_ex %>% 
  filter(stem == 'Retweets') %>% 
  ggplot() +
  aes(y = tidytext::reorder_within(lab, shap_value, stem), x = shap_value) +
  geom_col(aes(fill = sign), show.legend = FALSE) +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c('neg' = '#7a5193', 'pos' = '#ef5675')) +
  theme(
    panel.grid.major.y = element_blank()
  ) +
  facet_wrap(~stem, scales = 'free') +
  .add_common_layers() +
  theme(
    plot.subtitle = ggtext::element_markdown(),
    axis.text.x = element_blank()
  ) +
  labs(
    # x = 'SHAP value',
    x = NULL,
    subtitle = glue::glue('{preds_ex$lab_text}<br/><b><span style="color:#333333"><b>Favorites</b></span>, Actual / Predicted: {scales::comma(preds_ex$favorite_count)} / {scales::comma(preds_ex$favorite_pred)} | <b><span style="color:#333333"><b>Retweets</b></span>, Actual / Predicted: {scales::comma(preds_ex$retweet_count)} / {scales::comma(preds_ex$retweet_pred)}')
  )
viz_shap_ex

.f_save_shap <- partial(.save_plot, height = 8, width = 12, type = 'cairo', ... = )
.f_save_shap(viz_shap_ex)

viz_shap_agg <-
  shap_agg %>% 
  group_by(stem) %>% 
  filter(shap_value_rnk <= n_show) %>% 
  ungroup() %>% 
  ggplot() +
  aes(y = tidytext::reorder_within(lab, shap_value, stem), x = shap_value) +
  geom_col(aes(fill = stem), show.legend = FALSE) +
  tidytext::scale_y_reordered() +
  # .theme() +
  scale_fill_manual(values = c('Favorites' = '#003f5c', 'Retweets' = '#ffa600')) +
  theme(
    panel.grid.major.y = element_blank(),
    strip.text = element_text(size = 22),
    strip.background = element_rect(fill = NA)
  ) +
  facet_wrap(~stem, scales = 'free') +
  .add_common_layers() +
  labs(
    x = 'mean(|SHAP value|)',
    subtitle = 'Average across all tweets'
  )
viz_shap_agg

.f_save_shap(viz_shap_agg)

