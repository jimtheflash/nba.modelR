#' read data for building models
#' @param data_path chr path to data
#' @param filename_pattern chr regex for filename patterns to get
#' @param stitch_together bool should all the files be combined as one
#' @return data.frame if stitch_together is TRUE, else list
#' @export
get_data <- function(data_path = NULL, filename_pattern = '.csv$', stitch_together = TRUE) {

  if (is.null(data_path)) {
    stop('no data_path provided')
  }

  file_list <- list.files(data_path, pattern = filename_pattern)
  read_spec_csv <- function(x) {
    read.csv(file.path(data_path, x), colClasses = 'character')
    }
  df_list <- lapply(file_list, read_spec_csv)

  if (stitch_together) {
    return(do.call(rbind, df_list))
  }

  else {
    return(df_list)
  }
}
