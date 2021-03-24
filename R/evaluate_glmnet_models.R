evaluate_glmnet_models <- function(glmnet_model_objects,
                                   use_case = 'team_games') {

  if (use_case == 'team_games') {

    output_list <- list()
    for (i in names(glmnet_model_objects)) {

      obj <- glmnet_model_objects[[i]]

      test_predictors <- obj$test_predictors
      test_outcomes <- obj$test_outcomes

      mod_eval_list <- list()
      for (m in names(obj$model_list)) {

        mod  <- obj$model_list[[m]]
        preds <- as.numeric(predict(mod, test_predictors, type = 'response'))
        truth <- test_outcomes[[m]]
        mod_eval_output <- list(
            outcome = m,
            preds = preds,
            truth = truth)

        if (is.factor(test_outcomes[[m]])) {
          roc <- pROC::roc(truth, preds)
          mod_eval_output[['eval']] <- roc
          } else {
          corr <- cor(truth, preds)
          mod_eval_output[['eval']] <- corr
          }

        mod_eval_list[[m]] <- mod_eval_output

      }

      output_list[[i]] <- mod_eval_list
    }

    return(output_list)

  }
}


