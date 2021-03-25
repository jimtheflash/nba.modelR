# pipeline to build team-game models
rm(list = ls())
t1 <- Sys.time()
devtools::load_all()

# can set these in a yaml or something, and fix cases
datefields <- c('game_date')
numfields <- c('min', 'fgm', 'fga', 'fg_pct', 'fg3m', 'fg3a', 'fg3_pct', 'ftm', 'fta', 'ft_pct', 'oreb', 'dreb', 'reb', 'ast', 'tov', 'stl', 'blk', 'blka', 'pf', 'pfd', 'pts', 'nba_fantasy_pts', 'dd2', 'td3', 'fd', 'dk')
team_game_model_splits <- sort(c(1, 5, 11, 23))


## import and tidy
raw_data <- get_data(data_path = '/Users/jim/Documents/gambling_stuff/data/nba_gamelogs')
tidier_data <- tidyup_data(raw_data,
                           date_fields = datefields,
                           numeric_fields = numfields)

## wrangle
engineered_data <- engineer_data(tidier_data)
splitted_data <- split_data(engineered_data,
                            tg_mod_splits = team_game_model_splits)

## model
preproc_objects <- preproc_data(splitted_data) # recipes to recode, zv, nzv, lincomb, highcorr, scale, etc

glmnet_model_objects <- build_glmnet_models(preproc_objects)
ranger_model_objects <- build_ranger_models(preproc_objects)
earth_model_objects <- build_earth_models(preproc_objects)

glmnet_model_evals <- evaluate_glmnet_models(glmnet_model_objects)
ranger_model_evals <- evaluate_ranger_models(ranger_model_objects)
earth_model_evals <- evaluate_earth_models(earth_model_objects)

t2 <- Sys.time()
t2-t1

latest_schedule <- get_latest_team_game_schedule(schedule_path = '/Users/jim/Documents/gambling_stuff/data/nba_schedules')
tidier_schedule <- tidyup_schedule(latest_schedule)

new_data <- engineer_new_data(use_case = 'team_games',
                              schedule = tidier_schedule,
                              data = tidier_data)

preprocessed_new_data <- preproc_new_data(preproc_objects = preproc_objects,
                                          new_data = new_data)

glmnet_preds <- make_new_glmnet_predictions(new_data_list = preprocessed_new_data,
                                            glmnet_model_objects = glmnet_model_objects)


