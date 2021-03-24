build_glmnet_models_ha <- function(preprocessed_objects,
                                   use_case = 'team_games') {

  if (use_case == 'team_games') {

    output_list <- list()
    for (m in names(preprocessed_objects)) {
      ha_list <- list()
      for (ha in c('home', 'away')) {
        # subset the element of the list
        obj <- preprocessed_objects[[m]][[ha]]

        # use the recipe to subset terms
        rec <- obj$recipe
        ## subset predictors AS A MATRIX
        predictor_vars <- rec$term_info$variable[rec$term_info$role == 'predictor']
        predictors <- obj$training %>%
          dplyr::select(dplyr::any_of(predictor_vars)) %>%
          as.matrix()
        ## subset outcomes
        outcome_vars <- rec$term_info$variable[rec$term_info$role == 'outcome']
        outcomes <- obj$training %>%
          dplyr::select(dplyr::any_of(outcome_vars))

        # loop through the outcomes to build models depending on the outcome type
        model_list <- list()
        for (o in outcome_vars) {
          if (is.numeric(outcomes[[o]])) {
            mod <- glmnet::cv.glmnet(predictors, outcomes[[o]])
          }
          if (is.factor(outcomes[[o]])) {
            mod <- glmnet::cv.glmnet(predictors, outcomes[[o]], family = 'binomial')
          }
          model_list[[o]] <- mod
        }
        # start building output
        ha_list[[ha]][['model_list']] <- model_list
        ha_list[[ha]][['train_predictors']] <- predictors
        ha_list[[ha]][['train_outcomes']] <- outcomes

        # extract testing data for evaluations
        full_testing <- obj$testing
        test_predictors <- full_testing %>%
          dplyr::select(dplyr::any_of(predictor_vars)) %>%
          as.matrix()
        test_outcomes <- full_testing %>%
          dplyr::select(dplyr::any_of(outcome_vars))

        # finish building output
        ha_list[[ha]][['test_predictors']] <- test_predictors
        ha_list[[ha]][['test_outcomes']] <- test_outcomes
      }
      output_list[[m]] <- ha_list
    }
  }

  return(output_list)

}
