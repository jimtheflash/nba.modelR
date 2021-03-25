make_new_glmnet_predictions <- function(use_case = 'team_games',
                                        glmnet_model_objects, new_data_list) {

  if (use_case == 'team_games') {

    # loop through the new_data_list, select matching model, generate preds
    output_list <- list()
    for (nd in names(new_data_list)) {

      if (nrow(new_data_list[[nd]]) < 1) {
        message('no data for forecast for teams with ', nd, ' games played')
        next
      }

      ids <- new_data_list[[nd]] %>%
        dplyr::select(dplyr::ends_with('id'))

      predictors <- new_data_list[[nd]] %>%
        dplyr::select(-dplyr::ends_with('id')) %>%
        as.matrix()

      model_list <- glmnet_model_objects[[nd]][['model_list']]
      outcomes <- names(model_list)

      for (o in outcomes) {

        new_preds <- as.numeric(predict(model_list[[o]], predictors, type = 'response'))
        ids[[o]] <- new_preds

      }

      output_list[[nd]] <- ids

    }

    return(output_list)

  }

}
