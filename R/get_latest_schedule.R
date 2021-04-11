get_latest_schedule <- function(schedule_path = NULL) {

  schedules <- list.files(schedule_path, pattern = '.csv$')
  schedule_infos <- file.info(file.path(schedule_path, schedules))
  latest_schedule_path <- row.names(schedule_infos)[schedule_infos$ctime == max(schedule_infos$ctime)]
  latest_schedule <- read.csv(latest_schedule_path, colClasses = 'character')
  tidy_schedule <- latest_schedule %>%
    janitor::clean_names() %>%
    dplyr::transmute(game_id,
                     game_date = game_date_est,
                     home_team_id,
                     visitor_team_id)
  return(tidy_schedule)

}
