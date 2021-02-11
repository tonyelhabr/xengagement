

library(tidyverse)
library(rvest)
page <- 'https://teamcolorcodes.com/soccer/premier-league-color-codes/' %>% xml2::read_html()
page %>% rvest::html_nodes('body')
nodes <- page %>% html_nodes('body') %>% html_nodes('p') %>% html_nodes('.team-button')
nodes
team <- nodes %>% html_text()
styles <- nodes %>% html_attr('style')
styles %>% pluck('background-color')
styles
color_pri <-
  styles %>% 
  str_sub(19, 25)
color_sec <-
  styles %>% 
  str_replace('(^.*border-bottom[:] 4px solid )(#[A-Za-z0-9]+)(;)', '\\2') %>% 
  str_remove('\\scolor.*$')

res <-
  tibble(
    team = team,
    color_pri = color_pri,
    color_sec = color_sec
  ) %>% 
  arrange(team)
res %>% clipr::write_clip()
