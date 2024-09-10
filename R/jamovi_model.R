# Load necessary libraries
library(lavaan)
library(jamovi)

# Function to define the SEM model specification
define_model_specification <- function(jmv) {
  return(jmv$modelSpec)
}

check_and_adjust_covariance <- function(data) {
  cov_matrix <- cov(data)
  
  if (!is.positive.definite(cov_matrix)) {
    increment <- 1e-6
    while (!is.positive.definite(cov_matrix)) {
      cov_matrix <- cov_matrix + diag(nrow(cov_matrix)) * increment
      increment <- increment * 2 
    }
  }
  
  cov_matrix
}

generate_start_values <- function(model_specification, data) {
  
  fit <- tryCatch(
    {
      lavaan::sem(model_specification, data = data, do.fit = FALSE)
    },
    error = function(e) {
      message("Erro ao gerar valores iniciais: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(fit)) {
    return(NULL) 
  }
  
  lavaan::start(fit)
}

# Function to load and validate the dataset
load_and_validate_data <- function(jmv) {
  data <- jmv$data
  required_vars <- c("Mot1", "Mot2", "Mot3", "Des1", "Des2", "Des3")
  if (!all(required_vars %in% names(data))) {
    stop("Data validation failed: Required variables are missing.")
  }
  return(data)
}

# Function to compile the SEM model
compile_model <- function(model_specification, data) {
  observed_vars <- c("Mot1", "Mot2", "Mot3", "Des1", "Des2", "Des3")
  missing_vars <- observed_vars[!observed_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop("Variáveis observadas ausentes no conjunto de dados: ", paste(missing_vars, collapse = ", "))
  }
  
  data_scaled <- scale(data)
  cov_matrix <- check_and_adjust_covariance(data_scaled)
  start_values <- generate_start_values(model_specification, data_scaled)
  
  compiled_model <- tryCatch({
    lavaan::sem(model_specification, sample.cov = cov_matrix, sample.nobs = nrow(data_scaled), start = start_values, fixed.x = FALSE, control = list(maxiter = 1000, trace = 6)) 
  }, error = function(e) {
    message("Erro durante a compilação do modelo: ", e$message)
    return(NULL)
  }, warning = function(w) {
    message("Aviso durante a compilação do modelo: ", w$message)
    return(NULL)
  })
  
  if (!inherits(compiled_model, "lavaan")) {
    stop("Model compilation failed.")
  }
  return(compiled_model)
}

# Main function to execute the SEM analysis
execute_sem_analysis <- function(jmv) {
  data <- load_and_validate_data(jmv)
  model_specification <- define_model_specification(jmv)
  compiled_model <- compile_model(model_specification, data)
  
  jmv$semSummary$setContent(summary(compiled_model))
  
  image <- semPlot::semPaths(compiled_model, 
                      what = "path", 
                      style = "lisrel", 
                      layout = "tree", 
                      rotation = 0, 
                      curve = 0.5, 
                      nCharNodes = 10, 
                      sizeMan = 10, 
                      sizeLat = 10, 
                      color = "black", 
                      fade = FALSE)
  
  print(image)
  jmv$semPlot$setState(image)
}