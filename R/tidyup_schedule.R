tidyup_schedule <- function(schedule) {

  tidy_schedule <- schedule %>%
    janitor::clean_names() %>%
    dplyr::select(game_id, game_date_est, home_team_id, visitor_team_id)

  home <- tidy_schedule %>%
    dplyr::transmute(
      game_id = game_id,
      game_date = game_date_est,
      team_id = home_team_id,
      home_away = 'home'
    )

  away <- tidy_schedule %>%
    dplyr::transmute(
      game_id = game_id,
      game_date = game_date_est,
      team_id = visitor_team_id,
      home_away = 'away'
  )

  long_schedule <- dplyr::bind_rows(home, away)

  return(long_schedule)

}
