
today <- lubridate::today()
token <- get_twitter_token()
tm_accounts <- 
  list(
    'epl' = 786067957373894656, 
    'champ' = 786070708405272576 # ,
    # 'bund' = 786265012612456448,
    # 'seriea' = 787389659328327680,
    # 'ligue1' = 787391173576884224,
    # 'laliga' = 787391036179951616,
    # 'ered' = 786072885798506496
  ) %>% 
  purrr::map(rtweet::lists_members, token = token) %>% 
  purrr::map_dfr(dplyr::bind_rows, .id = 'lg') %>% 
  dplyr::select(lg, name, screen_name, user_id, followers_count, created_at) %>% 
  dplyr::arrange(dplyr::desc(followers_count))
tm_accounts$scraped_at <- today
tm_accounts

tm_mapping <-
  file.path('data-raw', 'tm_mapping.csv') %>% 
  readr::read_csv(
    col_types = readr::cols(
      .default = readr::col_character()
      # lg = readr::col_character(),
      # tm = readr::col_character(),
      # tm_understat = readr::col_character(),
      # tm_538 = readr::col_character(),
      # user_id = readr::col_character()
    )
  ) %>% 
  dplyr::select(-tm_understat) %>% 
  dplyr::filter(lg %in% c('epl', 'champ'))
tm_mapping

tm_accounts_mapping <-
  tm_mapping %>% 
  dplyr::left_join(
    tm_accounts %>% 
      dplyr::select(user_id, followers_count, created_at, scraped_at),
    by = 'user_id'
  )
tm_accounts_mapping

tm_corrections <-
  dplyr::tibble(
    tm = c('Spurs', 'Man Utd', 'C Palace'), # , 'Inter', 'Bayern', 'BVB', 'Dortmund', 'Leipzig'),
    tm_correct = c('Tottenham', 'Man United', 'Crystal Palace') # , 'Inter Milan', 'Bayern Munich', 'BVB Dortmund', 'BVB Dortmund', 'RB Leipzig')
  )

usethis::use_data(tm_accounts, tm_mapping, tm_accounts_mapping, tm_corrections, overwrite = TRUE, internal = TRUE)
