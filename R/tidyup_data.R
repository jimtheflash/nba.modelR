#' tidy up your data for modeling
#' @param data data.frame
#' @param fix_names bool
#' @param fix_dates bool
#' @param fix_numerics bool
#' @param date_fields chr fields to convert to dates in lowercase
#' @param numeric_fields chr fields to convert to numerics in lowercase
#' @return data.frame
#' @export
tidyup_data <- function(data = NULL,
                        fix_names = TRUE,
                        fix_dates = TRUE,
                        fix_numerics = TRUE,
                        # fields_to_nuke,
                        date_fields,
                        numeric_fields) {

  tidier_data <- data
  if (fix_names) {tidier_data <- janitor::clean_names(tidier_data)}
  if (fix_dates) {tidier_data[date_fields] <- lapply(tidier_data[date_fields], as.Date)}
  if (fix_numerics) {tidier_data[numeric_fields] <- lapply(tidier_data[numeric_fields], as.numeric)}

  # tidier_data <- tidier_data[, !(names(tidier_data) %in% fields_to_nuke)]

  return(tidier_data)

}
