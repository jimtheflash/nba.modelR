make_new_earth_predictions <- function(use_case = 'team_games',
                                        earth_model_objects, new_data_list) {

  if (use_case == 'team_games') {

    # loop through the new_data_list, select matching model, generate preds
    output_list <- list()
    for (nd in names(new_data_list)) {

      if (nrow(new_data_list[[nd]]) < 1) {
        message('no data for forecast for teams with ', nd, ' games played')
        next
      }

      ids <- new_data_list[[nd]] %>%
        dplyr::select(dplyr::ends_with('id')) %>%
        dplyr::mutate(range = nd,
                      type = 'earth')

      predictors <- new_data_list[[nd]] %>%
        dplyr::select(-dplyr::ends_with('id'))

      model_list <- earth_model_objects[[nd]][['model_list']]
      outcomes <- names(model_list)

      for (o in outcomes) {

        new_preds <- as.numeric(predict(model_list[[o]], predictors, type = 'response'))
        ids[[o]] <- new_preds

      }

      output_list[[nd]] <- ids

    }

    output <- do.call(rbind, output_list)

    return(output)

  }

}
