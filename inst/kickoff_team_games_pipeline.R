# pipeline to build team-game models
rm(list = ls())
t1 <- Sys.time()
devtools::load_all()

# can set these in a yaml or something, and fix cases
datefields <- c('game_date')
numfields <- c('min', 'fgm', 'fga', 'fg_pct', 'fg3m', 'fg3a', 'fg3_pct', 'ftm', 'fta', 'ft_pct', 'oreb', 'dreb', 'reb', 'ast', 'tov', 'stl', 'blk', 'blka', 'pf', 'pfd', 'pts', 'nba_fantasy_pts', 'dd2', 'td3', 'fd', 'dk')
team_game_model_splits <- sort(c(25, 81))

## import
raw_data <- get_data(data_path = '/Users/jim/Documents/gambling_stuff/data/nba_gamelogs')
### odds and lu
odds_data <- read.csv('/Users/jim/Documents/gambling_stuff/data/nba_lines/all_historical_lines.csv') %>%
  dplyr::mutate(team_id = as.character(team_id),
                game_date = as.Date(game_date))
team_lu <- read.csv('/Users/jim/Documents/gambling_stuff/data/lu/nba_team_lu.csv', colClasses = 'character')

## tidy
tidier_data <- tidyup_data(raw_data,
                           date_fields = datefields,
                           numeric_fields = numfields)

## wrangle
engineered_data <- engineer_data(tidier_data,
                                 tg_odds = odds_data)

## preprocess
### split into sets for modeling
splitted_data <- split_data(engineered_data,
                            tg_mod_splits = team_game_model_splits)
### recipes to recode, zv, nzv, lincomb, highcorr, scale, etc
preproc_objects <- preproc_data(splitted_data,
                                highcorr_thresh = .8,
                                na_perc = .1)

## model
# glmnet_model_objects <- build_glmnet_models(preproc_objects)
ranger_model_objects <- build_ranger_models(preproc_objects)
t1 <- Sys.time()
earth_model_objects <- build_earth_models(preproc_objects)
t2 <- Sys.time()
## evaluate
# glmnet_model_evals <- evaluate_glmnet_models(glmnet_model_objects)
ranger_model_evals <- evaluate_ranger_models(ranger_model_objects)
earth_model_evals <- evaluate_earth_models(earth_model_objects)

## get new schedule
latest_schedule <- get_latest_schedule(schedule_path = '/Users/jim/Documents/gambling_stuff/data/nba_schedules')

## get latest odds
odds <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1NIyRMfvQMExk4KLy1z04JLkScYwg_ifC_dy9EtnMBj8/edit#gid=1750804285',
                                  'output')
latest_odds <- tidyup_manual_odds(odds = odds)

# latest_odds <- tidyup_manual_odds('/Users/jim/Downloads/odds_inputs - output.csv')

## get new data
new_engineered_data <- engineer_new_data(use_case = 'team_games',
                                         schedule = latest_schedule,
                                         odds = latest_odds,
                                         data = tidier_data)

## preprocess new data
preprocessed_new_data <- preproc_new_data(preproc_objects = preproc_objects,
                                          new_data = new_engineered_data)

## make new predictions
# glmnet_preds <- make_new_glmnet_predictions(new_data_list = preprocessed_new_data,
#                                             glmnet_model_objects = glmnet_model_objects)
ranger_preds <- make_new_ranger_predictions(new_data_list = preprocessed_new_data,
                                            ranger_model_objects = ranger_model_objects)
earth_preds <- make_new_earth_predictions(new_data_list = preprocessed_new_data,
                                            earth_model_objects = earth_model_objects)

## tidy up predictions
tidy_preds <- tidyup_preds(
  # glmnet_preds,
  ranger_preds, earth_preds,
  team_lu = team_lu,
  schedule = latest_schedule,
  pf_weight = 1,
  pa_weight = 1)

View(tidy_preds)
# write_predictions(tidy_preds,
#                   write_dir = if(!dir.exists('/Users/jim/Documents/gambling_stuff/data/forecasts/nba_team_games/')))
t2 <- Sys.time()
t2-t1
