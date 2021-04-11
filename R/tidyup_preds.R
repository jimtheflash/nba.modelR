tidyup_preds <- function(...,
                         team_lu,
                         schedule,
                         reconcile_preds = TRUE,
                         pf_weight = 1,
                         pa_weight = 1) {


  preds_list <- list(...)
  all_preds <- do.call(rbind, preds_list)
  tidy_preds <- all_preds %>%
    dplyr::inner_join(team_lu, by = c('team_id' = 'team_id')) %>%
    dplyr::inner_join(team_lu %>% dplyr::transmute(opp_team_id = team_id,
                                                   opp_NBA_team_name = NBA_team_name),
                      by = c('opp_team_id' = 'opp_team_id')) %>%
    dplyr::group_by(
      game_id,
      team_id,
      opp_team_id,
      NBA_team_name,
      opp_NBA_team_name) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::matches('outcome'),
        list(mean = mean)))

  tidy_preds$home_away <- NA_character_
  for (i in 1:nrow(tidy_preds)) {
    tidy_preds$home_away[[i]] <- ifelse(tidy_preds$team_id[[i]] %in% unique(schedule$home_team_id), 'home', 'away')
  }

  if (reconcile_preds) {
    game_level <- tidy_preds %>%
      dplyr::group_by(game_id) %>%
      dplyr::mutate(total_win_perc = sum(win_outcome_mean),
                    adj_win_perc = win_outcome_mean / total_win_perc)

    wide_game_level <- game_level %>%
      dplyr::filter(home_away == 'home') %>%
      dplyr::transmute(
        game_id,
        home_team_id = team_id,
        home_team = NBA_team_name,
        home_pts_for = pts_game_outcome_mean,
        home_pts_allowed = pts_game_allowed_outcome_mean,
        home_adj_win_perc = adj_win_perc
      ) %>%
      dplyr::inner_join(
        game_level %>%
          dplyr::filter(home_away == 'away') %>%
          dplyr::transmute(
            game_id,
            away_team_id = team_id,
            away_team = NBA_team_name,
            away_pts_for = pts_game_outcome_mean,
            away_pts_allowed = pts_game_allowed_outcome_mean,
            away_adj_win_perc = adj_win_perc)) %>%
      dplyr::transmute(
        game_id,
        home_team_id,
        home_team,
        home_adj_pts_for = ((pf_weight * home_pts_for) + (pa_weight * away_pts_allowed)) / (pf_weight + pa_weight),
        home_adj_win_perc,
        away_team_id,
        away_team,
        away_adj_pts_for = ((pf_weight * away_pts_for) + (pa_weight * home_pts_allowed)) / (pf_weight + pa_weight),
        away_adj_win_perc,
        adj_total = home_adj_pts_for + away_adj_pts_for,
        adj_margin = home_adj_pts_for - away_adj_pts_for
      )

    return(wide_game_level)

  } else {

    return(tidy_preds)

  }

}
