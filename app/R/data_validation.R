# R/data_validation.R

validate_data <- function(df) {
  if (nrow(df)==0) stop("✗ Data frame is empty")
  req_cols <- c("Country","Category","View","Type","Latest")
  missing   <- setdiff(req_cols, names(df))
  if (length(missing)>0) stop(paste("✗ Missing columns:", paste(missing,collapse=", ")))
  # e.g. ensure numeric
  if (!is.numeric(df$Latest)) stop("✗ ‘Latest’ must be numeric")
  # add any other domain-specific checks here…
  return(TRUE)
}
