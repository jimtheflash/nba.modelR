#' split data for preprocessing and modeling
#' @param data data.frame
#' @param use_case chr
#' @param tg_seas_type chr
#' @param tg_mod_splits num
#' @param tg_testtrain_split_prop num
#' @return list
#' @export
split_data <- function(data,
                       use_case = 'team_games',
                       tg_seas_type = 'Regular Season',
                       tg_mod_splits = sort(c(1, 5, 11, 23)),
                       tg_testtrain_split_prop = .8) {

  if (use_case == 'team_games') {
    # filter here to regular season
    st_filtered <- data %>%
      dplyr::filter(season_type == tg_seas_type)

    # loop through game indices to select sets of features for eliminating nas
    split_df_list <- list()
    for (i in 1:length(tg_mod_splits)) {

      # setup game number filters
      game_num <- tg_mod_splits[[i]]
      if (i + 1 > length(tg_mod_splits)) {
        next_game_num <- max(st_filtered$season_type_game_index)
      } else {
        next_game_num <- tg_mod_splits[[i + 1]]
      }

      # filter data to only games within specified range, i.e. between min and max number of games
      ## TODO: FIX THIS IN ENGINEERING
      game_filtered <- st_filtered %>%
        dplyr::filter(season_type_game_index > game_num,
                      season_type_game_index <= next_game_num) %>%
        dplyr::filter(opp_season_type_game_index > game_num,
                      opp_season_type_game_index <= next_game_num)

      # identify and remove any oob columns, which should be all na
      oob_columns <- c(names(game_filtered)[apply(game_filtered, 2, function(x) all(is.na(x)))],
                       names(game_filtered)[grepl(paste0('_', next_game_num, '_'), names(game_filtered))])
      # pull out anything that matches next_game_num surrounded by underscores
      oob_col_filtered <- game_filtered %>%
        dplyr::select(-dplyr::any_of(oob_columns))

      # and finally split into testing and training for home/away
      training_data <- oob_col_filtered %>%
        dplyr::filter(as.numeric(game_date) < quantile(as.numeric(game_date), tg_testtrain_split_prop))
      testing_data <- oob_col_filtered %>%
        dplyr::filter(as.numeric(game_date) >= quantile(as.numeric(game_date), tg_testtrain_split_prop))

      # setup output
      output_name <- paste0(game_num + 1, '_', next_game_num)
      output_list <- list(
        train = training_data,
        test = testing_data
      )

      split_df_list[[output_name]] <- output_list

    }

  }

  return(split_df_list)

}
