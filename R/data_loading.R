load_and_validate_data <- function(jmv) {
  data <- jmv$data
  required_vars <- c("Mot1", "Mot2", "Mot3", "Des1", "Des2", "Des3")
  if (!all(required_vars %in% names(data))) {
    stop("Data validation failed: Required variables are missing.")
  }
  return(data)
}