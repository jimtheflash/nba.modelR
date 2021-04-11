# make an incrementor function to use with purrr::accumulate later
incrementor <- function(prev, new, growth = 1) {
  dplyr::if_else(new == 0|is.na(new), new, prev + growth)
}

# rolling window functions
rollsum_p <- function(x, p, ...) {
  slider::slide_vec(
    x,
    sum,
    .before = p,
    ...
  )
}

rollmean_p <- function(x, p, ...) {
  slider::slide_vec(
    x,
    mean,
    .before = p,
    ...
  )
}

rollmax_p <- function(x, p, ...) {
  slider::slide_vec(
    x,
    max,
    .before = p,
    ...
  )
}

rollmin_p <- function(x, p, ...) {
  slider::slide_vec(
    x,
    min,
    .before = p,
    ...
  )
}

rollmedian_p <- function(x, p, ...) {
  slider::slide_vec(
    x,
    median,
    .before = p,
    ...
  )
}

rollvar_p <- function(x, p, ...) {
  slider::slide_vec(
    x,
    var,
    .before = p,
    ...
  )
}

rollcount_days_p <- function(x, date_index, p, ...) {
  slider::slide_period_vec(
    x,
    date_index,
    "day",
    dplyr::n_distinct,
    .before = p,
    ...
  )
}

make_long_schedule <- function(schedule) {

  home <- schedule %>%
    dplyr::transmute(
      game_id,
      game_date,
      team_id = home_team_id,
      opp_team_id = visitor_team_id,
      home_away = 'home'
    )

  away <- schedule %>%
    dplyr::transmute(
      game_id,
      game_date,
      team_id = visitor_team_id,
      opp_team_id = home_team_id,
      home_away = 'away'
    )

  dplyr::bind_rows(home, away)

}
