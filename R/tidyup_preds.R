tidyup_preds <- function(...,
                         team_lu = NULL) {

  preds_list <- list(...)
  all_preds <- do.call(rbind, preds_list)
  tidy_preds <- all_preds %>%
    dplyr::inner_join(team_lu, by = c('team_id' = 'team_id')) %>%
    dplyr::inner_join(team_lu %>% dplyr::transmute(opp_team_id = team_id,
                                                   opp_team_name = team_name), by = c('opp_team_id' = 'opp_team_id')) %>%
    dplyr::group_by(
      game_id,
      team_id,
      opp_team_id,
      team_name,
      opp_team_name) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::matches('outcome'),
        list(mean = mean)))

  return(tidy_preds)

}
