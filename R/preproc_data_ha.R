preproc_data_ha <- function(data_list,
                            use_case = 'team_games') {

  if (use_case == 'team_games') {

    output_list <- list()

    for (i in names(data_list)) {

      testing_data <- data_list[[i]]$test
      training_data <- data_list[[i]]$train

      rec_outputs <- list()
      data_outputs <- list()
      for (ha in c('home', 'away')) {

        df <- training_data[[ha]]
        # nuke columns that are mostly NA
        df <- df[, !apply(df, 2, function(x) sum(is.na(x)) / length(x) > .05)]

        # build a recipe with the remaining columns' completed cases
        rec <- recipes::recipe(df) %>%
          recipes::update_role(dplyr::ends_with('outcome'), new_role = 'outcome') %>%
          recipes::update_role(-recipes::all_outcomes(), new_role = 'predictor') %>%
          recipes::update_role(season_year, team_name, opp_team_name, game_date, game_id, dplyr::matches('index'), new_role = 'id_vars') %>%
          recipes::update_role(season_type, home_away, new_role = 'split_vars') %>%
          recipes::step_date(game_date, features = "dow") %>%
          recipes::step_date(game_date, features = "month") %>%
          recipes::step_holiday(game_date) %>%
          recipes::step_rm(game_date) %>%
          recipes::step_dummy(recipes::all_predictors(), -recipes::all_numeric(), one_hot = TRUE) %>%
          recipes::step_naomit(recipes::all_predictors()) %>%
          recipes::step_zv(recipes::all_predictors()) %>%
          recipes::step_nzv(recipes::all_predictors(), freq_cut = 999, unique_cut = 0.2) %>%
          recipes::step_lincomb(recipes::all_predictors()) %>%
          recipes::step_corr(recipes::all_predictors(), threshold = 0.9) %>%
          recipes::prep()

        baked_train <- recipes::bake(rec, df)
        baked_test <- recipes::bake(rec, testing_data[[ha]])

        rec_outputs[[ha]] <- list(
          recipe = rec,
          training = baked_train,
          testing = baked_test)

      }

      output_list[[i]] <- rec_outputs
    }

  }

  return(output_list)
}
