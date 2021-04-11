tidyup_manual_odds <- function(odds = NULL,
                               manual_odds_path = NULL) {

  if (is.null(odds)) {
    man_odds <- read.csv(manual_odds_path)
  } else {
    man_odds <- odds
  }
  man_odds$team_id <- as.character(man_odds$team_id)
  man_odds$game_date <- NULL


  tidy_man_odds <- man_odds %>%
    dplyr::mutate(open_bookmaker_implied_pts = (open_bookmaker_total_line - open_bookmaker_spread_line) / 2,
           close_bookmaker_implied_pts = (close_bookmaker_total_line - close_bookmaker_spread_line) / 2,
           open_bookmaker_implied_pts_allowed = open_bookmaker_total_line - open_bookmaker_implied_pts,
           close_bookmaker_implied_pts_allowed = close_bookmaker_total_line - close_bookmaker_implied_pts) %>%
    dplyr::mutate(bookmaker_spread_line_delta = close_bookmaker_spread_line - open_bookmaker_spread_line,
           bookmaker_spread_line_delta_perc = bookmaker_spread_line_delta / open_bookmaker_spread_line,
           bookmaker_moneyline_delta = open_bookmaker_moneyline_line - close_bookmaker_moneyline_line,
           bookmaker_moneyline_delta_perc = bookmaker_moneyline_delta / open_bookmaker_moneyline_line,
           bookmaker_total_line_delta = close_bookmaker_total_line - open_bookmaker_total_line,
           bookmaker_total_line_delta_perc = bookmaker_total_line_delta / open_bookmaker_total_line)

  return(tidy_man_odds)

}
