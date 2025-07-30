# R/data_load.R

load_raw_data <- function(path = "data.rds") {
  if (!file.exists(path)) {
    stop("❌ 'data.rds' is missing. Please generate it from the CSV before publishing.")
  }
  readRDS(path)
}
