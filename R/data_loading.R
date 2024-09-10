# Load necessary libraries
library(lavaan)
library(NCA)
library(jamovi)

# Function to load and validate the dataset
load_and_validate_data <- function(jmv) {
  # Read data from the jamovi object
  data <- jmv$data
  
  # Define required variables for validation
  required_vars <- c("Mot1", "Mot2", "Mot3", "Des1", "Des2", "Des3")
  
  # Validate that all required variables are present in the dataset
  missing_vars <- required_vars[!required_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop("The following required variables are missing from the dataset: ", paste(missing_vars, collapse = ", "))
  }
  
  return(data)
}