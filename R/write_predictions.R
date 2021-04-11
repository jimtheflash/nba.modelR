write_predictions <- function(use_case = 'team_games',
                              preds,
                              write_dir,
                              tg_write_name = paste0('team_game_predictions_', as.numeric(Sys.time()), '.csv')) {

  write_path <- file.path(write_dir, tg_write_name)
  write.csv(preds, write_path)
  message('successfully wrote predictions to ', write_path)

}
