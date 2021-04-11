#' engineer your data for a use case
#' @importFrom magrittr %>%
#' @param data data.frame data to be engineered
#' @param use_case chrwhich use case to engineer; currently just supports 'team_games'
#' @param tg_odds chr data.frame with odds
#' @return engineered data
#' @export
engineer_data <- function(data,
                          use_case = 'team_games',
                          tg_odds) {

  if (use_case == 'team_games') {

    # TODO: there are opportunities to further modularize this, but for now sections it is

    # this chain results in df at player-game level with features engineered that won't be aggregated later
    eng_data <- data %>%
      dplyr::mutate(home_away = dplyr::if_else(grepl('@', matchup), 'away', 'home'))


# team_games1 -------------------------------------------------------------

    # this chain results in df at the team-game level with minimal engineering
    team_games1 <- eng_data %>%
      dplyr::group_by(season_year, season_type, team_id, game_id, game_date, wl, home_away) %>%
      dplyr::summarise(
        # first do all the numeric columns
        dplyr::across(
          tidyselect:::where(is.numeric),
          sum,
          .names = '{.col}_game'
        ),
        # see how many players in rotation exceeded minutes thresholds
        # TODO: FUNCTIONALIZE CORRECTLY
        players_gt_0min_game = dplyr::n_distinct(player_id[min > 0]),
        players_gt_10min_game = dplyr::n_distinct(player_id[min > 10]),
        players_gt_20min_game = dplyr::n_distinct(player_id[min > 20]),
        .groups = 'keep'
      ) %>%
      # calculate team-game ratios and percentages
      dplyr::mutate(ft_perc_game = ftm_game / fta_game,
                    fg_perc_game = fgm_game / fga_game,
                    fg3_perc_game = fg3m_game / fg3a_game,
                    ast_tov_ratio_game = ast_game / tov_game,
                    fg3a_fga_ratio_game = fg3a_game / fga_game,
                    fg3m_fgm_ratio_game = fg3m_game / fgm_game,
                    oreb_perc_game = oreb_game / reb_game,
                    ft_points_ratio_game = ftm_game / pts_game) %>%
      dplyr::mutate(overtime = dplyr::if_else(min_game > 240, 1, 0),
                    count_overtime = dplyr::case_when(
                      overtime == 0 ~ 0,
                      overtime > 0 ~ (min_game - 240) / 25)) %>%
      dplyr::select(-min_game) %>%
      dplyr::ungroup()

# team_games2 -------------------------------------------------------------

    # this chain applies a bunch of order-dependent and rolling window functions within team-seasons
    ## make some function lists
    rwl <- make_rolling_window_list()
    rolling_funs_list <- purrr::flatten(rwl)

    team_games2 <- team_games1 %>%
      dplyr::arrange(season_year, team_id, game_date) %>%
      dplyr::group_by(season_year, season_type, team_id) %>%
      dplyr::mutate(season_type_game_index = dplyr::row_number()) %>%
      dplyr::mutate(
        # TODO: ensure we can actually look into the future before making this feature available
        # b2b_g1 = dplyr::case_when(dplyr::lead(game_date) == game_date + 1 ~ 1,
        #                           dplyr::lead(game_date) != game_date + 1 ~ 0,
        #                           row_number() == max(row_number()) ~ 0),
        b2b_g2 = dplyr::case_when(dplyr::lag(game_date) == game_date - 1 ~ 1,
                                  dplyr::lag(game_date) != game_date - 1 ~ 0,
                                  dplyr::row_number() == 1 ~ 0)) %>%
      ## make some temporary features for easier incrementing and counting events
      dplyr::mutate(
        win = as.numeric(wl == 'W'),
        lose = as.numeric(wl == 'L'),
        home = as.numeric(home_away == 'home'),
        away = as.numeric(home_away == 'away'),
        dplyr::across(
          c(win, lose, home, away),
          rwl$rollsum_list)) %>%
      # streaks
      dplyr::mutate(
        dplyr::across(
          c(win, lose, home, away),
          list(streak = ~purrr::accumulate(.x, incrementor)))) %>%
      # now everything blows up with the boxscore stats
      dplyr::mutate(
        dplyr::across(
          dplyr::ends_with('_game'),
          rolling_funs_list)) %>%
      # and we can also calc things like games in last period of days
      # but this is a 2-arg function and i'm not sure how to do that in an across() call
      dplyr::mutate(
        dplyr::across(
          game_id,
          list(
            last5days = ~rollcount_days_p(.x, game_date, 4, .complete = TRUE),
            last11days = ~rollcount_days_p(.x, game_date, 10, .complete = TRUE),
            last23days = ~rollcount_days_p(.x, game_date, 22, .complete = TRUE)
          ))) %>%
      dplyr::select(-home, -away, -win, -lose) %>%
      dplyr::ungroup()

# team_games3 -------------------------------------------------------------

    # this chain joins the team_games data back onto itself but with the opponent's team_id, to get defensive (i.e. 'allowed') metrics
    team_games3 <- team_games2 %>%
      dplyr::mutate(join_var = dplyr::if_else(home_away == 'home', 'away', 'home')) %>%
      dplyr::inner_join(
        team_games2 %>%
          dplyr::select(-season_year, -season_type, -wl,
                        -dplyr::matches('overtime'),
                        -dplyr::matches('game_index')),
        by = c('game_id' = 'game_id',
               'game_date' = 'game_date',
               'join_var' = 'home_away')) %>%
      dplyr::select(-join_var) %>%
      dplyr::ungroup()

    ## fix the names to reflect what they actually mean
    fixed_names3 <- gsub('.x', '', names(team_games3), fixed = TRUE)
    fixed_names3 <- gsub('.y', '_allowed', fixed_names3, fixed = TRUE)
    # fixed_names3 <- gsub('^(players_gt*.*)_allowed$', 'opp_\\1', fixed_names3)
    # fixed_names3 <- sub('team_id_allowed', 'opp_team_id', fixed_names3)
    names(team_games3) <- fixed_names3

# team_games4 -------------------------------------------------------------

    # this chain engineers game-level data (i.e. team vs. opponent stuff)
    team_games4 <- team_games3 %>%
      dplyr::mutate(score_margin_game = pts_game - pts_game_allowed) %>%
      dplyr::group_by(season_year, season_type, team_id) %>%
      dplyr::mutate(
        dplyr::across(score_margin_game, rolling_funs_list)) %>%
      dplyr::ungroup()

# team_games5 -------------------------------------------------------------

    # this chain joins opponent's offensive and defensive historical performance
    team_games5 <- team_games4 %>%
      dplyr::inner_join(
        team_games4 %>%
          dplyr::select(-season_year, -season_type, -game_date, -home_away, -wl),
        by = c('game_id' = 'game_id',
               'team_id' = 'team_id_allowed'))

    # fix the names again
    fixed_names5 <- gsub('\\.x$', '', names(team_games5))
    fixed_names5 <- gsub('^(.*)\\.y$', 'opp_\\1', fixed_names5)
    names(team_games5) <- fixed_names5

# team_games6 -------------------------------------------------------------

    # this chain lags the predictors that need to be lagged to align with information at prediction time
    team_games6 <- team_games5 %>%
      dplyr::arrange(season_year, team_id, game_date) %>%
      dplyr::group_by(season_year, season_type, team_id) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::matches('_game$|_allowed$|_roll|_streak|players_gt|overtime|ratio|perc|wl'),
          list(lagged = ~dplyr::lag(.x)))) %>%
      dplyr::mutate(
        pts_game_outcome = pts_game,
        pts_game_allowed_outcome = pts_game_allowed,
        score_margin_game_outcome = score_margin_game,
        # overtime_outcome = factor(overtime == 1),
        win_outcome = factor(wl == 'W')) %>%
      dplyr::select(
        -wl,
        -dplyr::matches('_[1-9]?.$'),
        -dplyr::matches('team_id_'),
        -dplyr::ends_with('_allowed'),
        -dplyr::ends_with('_game'),
        -dplyr::ends_with('streak'),
        -dplyr::ends_with('rotation'),
        -dplyr::ends_with('overtime')) %>%
      dplyr::ungroup()



# team_games7 -------------------------------------------------------------

    # this chain adds odds; model performance drops ~10% without this
    team_games7 <- team_games6 %>%
      dplyr::inner_join(odds_data) %>%
      dplyr::select(-dplyr::any_of('bookmaker_spread_line_delta_perc')) #TODO: fix this in the odds generation files

# team_games8 -------------------------------------------------------------

    # this chain engineers team-opponent rolling features
    team_games8 <- team_games7 %>%
      dplyr::mutate(
        delta_pts_game_lagged = pts_game_lagged - opp_pts_game_lagged,
        delta_pts_game_rollmean_5_lagged = pts_game_rollmean_5_lagged - opp_pts_game_rollmean_5_lagged,
        delta_pts_game_rollmean_11_lagged = pts_game_rollmean_11_lagged - opp_pts_game_rollmean_11_lagged,
        delta_pts_game_rollmean_23_lagged = pts_game_rollmean_23_lagged - opp_pts_game_rollmean_23_lagged,

        delta_fg_pct_game_lagged = fg_pct_game_lagged - opp_fg_pct_game_lagged,
        delta_fg_pct_game_rollmean_5_lagged = fg_pct_game_rollmean_5_lagged - opp_fg_pct_game_rollmean_5_lagged,
        delta_fg_pct_game_rollmean_11_lagged = fg_pct_game_rollmean_11_lagged - opp_fg_pct_game_rollmean_11_lagged,
        delta_fg_pct_game_rollmean_23_lagged = fg_pct_game_rollmean_23_lagged - opp_fg_pct_game_rollmean_23_lagged,

        delta_fg3_pct_game_lagged = fg3_pct_game_lagged - opp_fg3_pct_game_lagged,
        delta_fg3_pct_game_rollmean_5_lagged = fg3_pct_game_rollmean_5_lagged - opp_fg3_pct_game_rollmean_5_lagged,
        delta_fg3_pct_game_rollmean_11_lagged = fg3_pct_game_rollmean_11_lagged - opp_fg3_pct_game_rollmean_11_lagged,
        delta_fg3_pct_game_rollmean_23_lagged = fg3_pct_game_rollmean_23_lagged - opp_fg3_pct_game_rollmean_23_lagged,

        delta_reb_game_lagged = reb_game_lagged - opp_reb_game_lagged,
        delta_reb_game_rollmean_5_lagged = reb_game_rollmean_5_lagged - opp_reb_game_rollmean_5_lagged,
        delta_reb_game_rollmean_11_lagged = reb_game_rollmean_11_lagged - opp_reb_game_rollmean_11_lagged,
        delta_reb_game_rollmean_23_lagged = reb_game_rollmean_23_lagged - opp_reb_game_rollmean_23_lagged,

        delta_ast_game_lagged = ast_game_lagged - opp_ast_game_lagged,
        delta_ast_game_rollmean_5_lagged = ast_game_rollmean_5_lagged - opp_ast_game_rollmean_5_lagged,
        delta_ast_game_rollmean_11_lagged = ast_game_rollmean_11_lagged - opp_ast_game_rollmean_11_lagged,
        delta_ast_game_rollmean_23_lagged = ast_game_rollmean_23_lagged - opp_ast_game_rollmean_23_lagged,

        delta_stl_game_lagged = stl_game_lagged - opp_stl_game_lagged,
        delta_stl_game_rollmean_5_lagged = stl_game_rollmean_5_lagged - opp_stl_game_rollmean_5_lagged,
        delta_stl_game_rollmean_11_lagged = stl_game_rollmean_11_lagged - opp_stl_game_rollmean_11_lagged,
        delta_stl_game_rollmean_23_lagged = stl_game_rollmean_23_lagged - opp_stl_game_rollmean_23_lagged,

        delta_tov_game_lagged = tov_game_lagged - opp_tov_game_lagged,
        delta_tov_game_rollmean_5_lagged = tov_game_rollmean_5_lagged - opp_tov_game_rollmean_5_lagged,
        delta_tov_game_rollmean_11_lagged = tov_game_rollmean_11_lagged - opp_tov_game_rollmean_11_lagged,
        delta_tov_game_rollmean_23_lagged = tov_game_rollmean_23_lagged - opp_tov_game_rollmean_23_lagged,

        delta_pf_game_lagged = pf_game_lagged - opp_pf_game_lagged,
        delta_pf_game_rollmean_5_lagged = pf_game_rollmean_5_lagged - opp_pf_game_rollmean_5_lagged,
        delta_pf_game_rollmean_11_lagged = pf_game_rollmean_11_lagged - opp_pf_game_rollmean_11_lagged,
        delta_pf_game_rollmean_23_lagged = pf_game_rollmean_23_lagged - opp_pf_game_rollmean_23_lagged,

        delta_pfd_game_lagged = pfd_game_lagged - opp_pfd_game_lagged,
        delta_pfd_game_rollmean_5_lagged = pfd_game_rollmean_5_lagged - opp_pfd_game_rollmean_5_lagged,
        delta_pfd_game_rollmean_11_lagged = pfd_game_rollmean_11_lagged - opp_pfd_game_rollmean_11_lagged,
        delta_pfd_game_rollmean_23_lagged = pfd_game_rollmean_23_lagged - opp_pfd_game_rollmean_23_lagged,

        delta_pts_game_lagged = pts_game_lagged - opp_pts_game_lagged,
        delta_pts_game_rollmean_5_lagged = pts_game_rollmean_5_lagged - opp_pts_game_rollmean_5_lagged,
        delta_pts_game_rollmean_11_lagged = pts_game_rollmean_11_lagged - opp_pts_game_rollmean_11_lagged,
        delta_pts_game_rollmean_23_lagged = pts_game_rollmean_23_lagged - opp_pts_game_rollmean_23_lagged,

        delta_pts_game_vs_opp_pts_game_allowed_lagged = pts_game_lagged - opp_pts_game_allowed_lagged,
        delta_pts_game_vs_opp_pts_game_allowed_rollmean_5_lagged = pts_game_rollmean_5_lagged - opp_pts_game_rollmean_5_allowed_lagged,
        delta_pts_game_vs_opp_pts_game_allowed_rollmean_11_lagged = pts_game_rollmean_11_lagged - opp_pts_game_rollmean_11_allowed_lagged,
        delta_pts_game_vs_opp_pts_game_allowed_rollmean_23_lagged = pts_game_rollmean_23_lagged - opp_pts_game_rollmean_23_allowed_lagged,

        delta_pts_game_allowed_vs_opp_pts_game_lagged = pts_game_allowed_lagged - opp_pts_game_lagged,
        delta_pts_game_allowed_vs_opp_pts_game_rollmean_5_lagged = pts_game_rollmean_5_allowed_lagged - opp_pts_game_rollmean_5_lagged,
        delta_pts_game_allowed_vs_opp_pts_game_rollmean_11_lagged = pts_game_rollmean_11_allowed_lagged - opp_pts_game_rollmean_11_lagged,
        delta_pts_game_allowed_vs_opp_pts_game_rollmean_23_lagged = pts_game_rollmean_23_allowed_lagged - opp_pts_game_rollmean_23_lagged
      )

    output <- team_games8

  }

  return(output)

}
