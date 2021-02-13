
today <- lubridate::today()
token <- get_twitter_token()
team_accounts <- 
  list(
    'epl' = 786067957373894656, 
    'champ' = 786070708405272576
  ) %>% 
  purrr::map(rtweet::lists_members, token = token) %>% 
  purrr::map_dfr(dplyr::bind_rows, .id = 'league') %>% 
  dplyr::select(league, name, screen_name, user_id, followers_count, created_at) %>% 
  dplyr::arrange(dplyr::desc(followers_count))
team_accounts$scraped_at <- today
team_accounts

team_mapping <-
  file.path('data-raw', 'team_mapping.csv') %>% 
  readr::read_csv(
    col_types = readr::cols(
      .default = readr::col_character()
    )
  ) %>% 
  dplyr::select(-team_understat) %>% 
  dplyr::filter(league %in% c('epl', 'champ'))
team_mapping

team_accounts_mapping <-
  team_mapping %>% 
  dplyr::left_join(
    team_accounts %>% 
      dplyr::select(user_id, followers_count, created_at, scraped_at),
    by = 'user_id'
  )
team_accounts_mapping

team_corrections <-
  dplyr::tibble(
    team = c('Spurs', 'Man Utd', 'C Palace'),
    team_correct = c('Tottenham', 'Man United', 'Crystal Palace')
  )

usethis::use_data(team_accounts, team_mapping, team_accounts_mapping, team_corrections, overwrite = TRUE, internal = TRUE)
