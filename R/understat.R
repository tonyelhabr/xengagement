
#' #' @noRd
#' .retrieve_understatr <- function() {
#' 
#'   lg_tm_stats_nested <-
#'     tidyr::crossing(
#'       lg = 'epl',
#'       yr = 2019L:2020L
#'     ) %>% 
#'     dplyr::mutate(data = purrr::map2(lg, yr, understatr::get_league_teams_stats))
#'   lg_tm_stats_nested
#'   
#'   lg_tm_stats <- 
#'     lg_tm_stats_nested %>% 
#'     tidyr::unnest(data) %>% 
#'     dplyr::select(
#'       lg,
#'       yr,
#'       date,
#'       tm = team_name,
#'       # # team_id, 
#'       g = scored,
#'       ga = missed,
#'       # xg = xG,
#'       # xga = xGA,
#'       # npxg = npxG,
#'       # npxga = npxGA,
#'       # npxgd = npxGD,
#'       # xpts = xpts,
#'       # h_a,
#'       w = wins, 
#'       l = loses, 
#'       d = draws, 
#'       result,
#'       pts
#'     ) %>% 
#'     dplyr::mutate(gd = g - ga) %>% 
#'     dplyr::arrange(lg, yr, date, tm) %>% 
#'     dplyr::group_by(lg, yr, tm) %>% 
#'     dplyr::mutate(
#'       wk = cumsum(w + l + d)
#'     ) %>% 
#'     dplyr::ungroup() %>% 
#'     dplyr::select(lg, yr, wk, dplyr::everything()) %>% 
#'     dplyr::group_by(lg, yr, tm, wk) %>% 
#'     dplyr::mutate(
#'       rnk = dplyr::min_rank(dplyr::desc(pts)),
#'       rnk_d = dplyr::row_number(dplyr::desc(gd))
#'     ) %>% 
#'     dplyr::ungroup() %>% 
#'     dplyr::mutate(
#'       rnk = dplyr::row_number(rnk + rnk_d)
#'     ) %>% 
#'     dplyr::select(-rnk_d)
#'   lg_tm_stats
#' }
#' lg_tm_stats %>% count(tm) %>% clipr::write_clip()
