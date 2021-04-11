#' preprocess data for nba models
#'
#' @param data_list list of data.frames
#' @param use_case chr which use case to go with; this should change to methods at some point
#' @param na_perc num proportion of a column's values that are NA for that column to be nuked
#' @param highcorr_thresh num threshold for pairwise correlations between columns, used in recipes::step_corr()
#' @return list of data for modeling
#' @export

preproc_data <- function(data_list,
                         use_case = 'team_games',
                         na_perc = .1,
                         highcorr_thresh = .9) {

  if (use_case == 'team_games') {

    output_list <- list()

    for (i in names(data_list)) {

      # nuke mostly empty columns
      df <- data_list[[i]]$train[, !apply(data_list[[i]]$train, 2, function(x) sum(is.na(x)) / length(x) > na_perc)]
      df <- na.omit(df)

      # build a recipe with the remaining columns' completed cases
      rec <- recipes::recipe(df) %>%
        recipes::update_role(dplyr::ends_with('outcome'), new_role = 'outcome') %>%
        recipes::update_role(-recipes::all_outcomes(), new_role = 'predictor') %>%
        recipes::update_role(season_year, game_date, dplyr::matches('team'), dplyr::matches('_id'), dplyr::matches('_name'), dplyr::matches('_index'), new_role = 'id_vars') %>%
        recipes::update_role(season_type, new_role = 'split_vars') %>%
        recipes::step_zv(recipes::all_predictors()) %>%
        recipes::step_date(game_date, features = "dow") %>%
        recipes::step_date(game_date, features = "month") %>%
        recipes::step_holiday(game_date) %>%
        recipes::step_rm(game_date) %>%
        recipes::step_naomit(recipes::all_predictors()) %>%
        recipes::step_string2factor(home_away, wl_lagged) %>%
        recipes::step_dummy(recipes::all_predictors(), -recipes::all_numeric()) %>%
        recipes::step_lincomb(recipes::all_predictors()) %>%
        recipes::step_corr(recipes::all_predictors(), threshold = highcorr_thresh) %>%
        recipes::prep(strings_as_factors = FALSE)

        baked_train <- recipes::bake(rec, df)
        baked_test <- recipes::bake(rec, data_list[[i]]$test)

         output_list[[i]] <- list(
          recipe = rec,
          training = baked_train,
          testing = baked_test)
        }
    }

  return(output_list)
}
