
today <- lubridate::today()
tm_accounts <- 
  list(
    'epl' = 786067957373894656, 
    'champ' = 786070708405272576,
    'bund' = 786265012612456448,
    'seriea' = 787389659328327680,
    'ligue1' = 787391173576884224,
    'laliga' = 787391036179951616,
    'ered' = 786072885798506496
  ) %>% 
  map(rtweet::lists_members) %>% 
  map_dfr(bind_rows, .id = 'lg') %>% 
  select(lg, name, screen_name, user_id, followers_count, created_at) %>% 
  arrange(desc(followers_count))
tm_accounts$scraped_at <- today
tm_accounts

tm_mapping <-
  file.path('data-raw', 'team_account_mapping.csv') %>% 
  read_csv(
    col_types = cols(
      lg = col_character(),
      tm = col_character(),
      user_id = col_character()
    )
  )
tm_mapping

tm_accounts_mapping <-
  tm_mapping %>% 
  left_join(
    tm_accounts %>% 
      select(user_id, followers_count, created_at, scraped_at),
    by = 'user_id'
  )
tm_accounts_mapping

usethis::use_data(tm_accounts, tm_mapping, tm_accounts_mapping, overwrite = TRUE, internal = TRUE)
