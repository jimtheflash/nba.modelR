preproc_new_data <- function(use_case = 'team_games',
                             preproc_objects, new_data) {

  if (use_case == 'team_games') {
browser()
    output_list <- list()

    # setup some ranges of max and min games for models to be used
    ranges <- names(preproc_objects)
    split_ranges <- strsplit(ranges, '_')
    names(split_ranges) <- ranges

    # loop through the preproc objects and preproc the new_data accordingly
    for (i in names(preproc_objects)) {

      preproc_obj <- preproc_objects[[i]]
      game_range <- split_ranges[[i]]
      min_games <- as.numeric(game_range[[1]])
      max_games <- as.numeric(game_range[[2]])

      data_to_preproc <- new_data %>%
        dplyr::filter(season_type_game_index >= min_games,
                      season_type_game_index <= max_games,
                      opp_season_type_game_index >= min_games,
                      opp_season_type_game_index <= max_games)

      preprocessed_new_data <- recipes::bake(preproc_obj$recipe, data_to_preproc, recipes::all_predictors())
      id_cols <- data_to_preproc %>% dplyr::select(dplyr::ends_with('id'))
      output_list[[i]] <- dplyr::bind_cols(id_cols, preprocessed_new_data)

    }

    return(output_list)
  }

}

