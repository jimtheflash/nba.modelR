#' split data for preprocessing and modeling
#' @param data data.frame
#' @param use_case chr
#' @param tg_seas_type chr
#' @param tg_nums num
#' @param tg_testtrain_split_prop num
#' @return list
#' @export
split_data <- function(data,
                       use_case = 'team_games',
                       tg_seas_type = 'Regular Season',
                       tg_nums = sort(c(1, 5, 11, 23)),
                       tg_testtrain_split_prop = .9) {

  if (use_case == 'team_games') {
    # filter here to regular season
    st_filtered <- data %>%
      dplyr::filter(season_type == tg_seas_type)

    # loop through game indices to select sets of features for eliminating nas
    split_df_list <- list()
    for (i in 1:length(tg_nums)) {

      # setup game number filters
      game_num <- tg_nums[[i]]
      if (i + 1 > length(tg_nums)) {
        next_game_num <- max(st_filtered$season_type_game_index)
      } else {
        next_game_num <- tg_nums[[i + 1]]
      }

      # filter data to only games within specified range, i.e. between min and max number of games
      game_filtered <- st_filtered %>%
        dplyr::filter(season_type_game_index > game_num,
                      season_type_game_index <= next_game_num) %>%
        # TODO: see if this actually necessary (probably is, maybe need to remove or add imputation in preproc...)
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
      output_name <- paste0('data_for_modeling_gms_', game_num + 1, '_thru_', next_game_num)
      output_list <- list(
        train = training_data,
        test = testing_data
      )

      split_df_list[[output_name]] <- output_list

    }

  }

  return(split_df_list)

}
