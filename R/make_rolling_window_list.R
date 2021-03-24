#' make lists of rolling window functions
#' @param countdays bool should windows only be counts of days
#' @return list
#' @export
make_rolling_window_list <- function(countdays = FALSE) {

  output_list <- list(

## turns out, all these effing functions need to be switched to 1 unit fewer than their real window...
# mean --------------------------------------------------------------------
    rollmean_list = list(
      rollmean_5 = ~rollmean_p(.x, 4, .complete = TRUE),
      rollmean_11 = ~rollmean_p(.x, 10, .complete = TRUE),
      rollmean_23 = ~rollmean_p(.x, 22, .complete = TRUE)),

# max ---------------------------------------------------------------------
    rollmax_list = list(
      rollmax_5 = ~rollmax_p(.x, 4, .complete = TRUE),
      rollmax_11 = ~rollmax_p(.x, 10, .complete = TRUE),
      rollmax_23 = ~rollmax_p(.x, 22, .complete = TRUE)),

# min ---------------------------------------------------------------------
    rollmin_list = list(
      rollmin_5 = ~rollmin_p(.x, 4, .complete = TRUE),
      rollmin_11 = ~rollmin_p(.x, 10, .complete = TRUE),
      rollmin_23 = ~rollmin_p(.x, 22, .complete = TRUE)),

# median ------------------------------------------------------------------
    rollmedian_list = list(
      rollmedian_5 = ~rollmedian_p(.x, 4, .complete = TRUE),
      rollmedian_11 = ~rollmedian_p(.x, 10, .complete = TRUE),
      rollmedian_23 = ~rollmedian_p(.x, 22, .complete = TRUE)),

# variance ----------------------------------------------------------------
    rollvar_list = list(
      rollvar_5 = ~rollvar_p(.x, 4, .complete = TRUE),
      rollvar_11 = ~rollvar_p(.x, 10, .complete = TRUE),
      rollvar_23 = ~rollvar_p(.x, 22, .complete = TRUE)),

# sum ---------------------------------------------------------------------
    rollsum_list = list(
      rollsum_5 = ~rollsum_p(.x, 4, .complete = TRUE),
      rollsum_11 = ~rollsum_p(.x, 10, .complete = TRUE),
      rollsum_23 = ~rollsum_p(.x, 22, .complete = TRUE)))

# countdays ---------------------------------------------------------------

  if (countdays) {
    output_list <- list(
      last3days = ~rollcount_days_p(.x, .y, 3, .complete = TRUE),
      last7days = ~rollcount_days_p(.x, .y, 7, .complete = TRUE),
      last11days = ~rollcount_days_p(.x, .y, 11, .complete = TRUE))

  }

# output ------------------------------------------------------------------


  return(output_list)

}
