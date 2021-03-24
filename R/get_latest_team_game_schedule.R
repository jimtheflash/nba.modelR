get_latest_team_game_schedule <- function(schedule_path = NULL) {

  schedules <- list.files(schedule_path, pattern = '.csv$')
  schedule_infos <- file.info(file.path(schedule_path, schedules))
  latest_schedule_path <- row.names(schedule_infos)[schedule_infos$ctime == max(schedule_infos$ctime)]
  latest_schedule <- read.csv(latest_schedule_path, colClasses = 'character')
  return(latest_schedule)

}
