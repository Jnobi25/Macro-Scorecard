# R/dataprep.R

prepare_data <- function() {
  df <- load_raw_data()
  validate_data(df)
  df
}