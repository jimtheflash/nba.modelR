build_earth_models <- function(preprocessed_objects,
                               use_case = 'team_games') {

  if (use_case == 'team_games') {

    output_list <- list()
    for (m in names(preprocessed_objects)) {

      obj <- preprocessed_objects[[m]]
      # use the recipe to subset terms
      rec <- obj$recipe
      ## subset predictors
      predictor_vars <- rec$term_info$variable[rec$term_info$role == 'predictor']
      predictors <- obj$training %>%
        dplyr::select(dplyr::any_of(predictor_vars))
      ## subset outcomes
      outcome_vars <- rec$term_info$variable[rec$term_info$role == 'outcome']
      outcomes <- obj$training %>%
        dplyr::select(dplyr::any_of(outcome_vars))
      # loop through the outcomes to build models depending on the outcome type
      model_list <- list()
      for (o in outcome_vars) {
        if (is.numeric(outcomes[[o]])) {
          mod <- earth::earth(x = predictors, y = outcomes[[o]], degree = 1, nfold = 3)
        } else if (is.factor(outcomes[[o]])) {
          mod <- earth::earth(x = predictors, y = outcomes[[o]], degree = 1, glm = list(family = binomial), nfold = 3)
        }
        model_list[[o]] <- mod
      }
      # start building output
      output_list[[m]][['model_list']] <- model_list
      output_list[[m]][['train_predictors']] <- predictors
      output_list[[m]][['train_outcomes']] <- outcomes
      # extract testing data for evaluations
      full_testing <- obj$testing
      test_predictors <- full_testing %>%
        dplyr::select(dplyr::any_of(predictor_vars))
      test_outcomes <- full_testing %>%
        dplyr::select(dplyr::any_of(outcome_vars))
      # finish building output
      output_list[[m]][['test_predictors']] <- test_predictors
      output_list[[m]][['test_outcomes']] <- test_outcomes
    }
  }

  return(output_list)

}
