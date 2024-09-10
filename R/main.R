# Load required libraries
library(lavaan)
library(NCA)
library(jamovi)

# Function to load and validate data
load_and_validate_data <- function(jmv) {
  data <- jmv$data
  required_vars <- c("Mot1", "Mot2", "Mot3", "Des1", "Des2", "Des3")
  if (!all(required_vars %in% names(data))) {
    stop("Data validation failed: Required variables are missing.")
  }
  return(data)
}

# Function to define the SEM model specification
define_model_specification <- function(jmv) {
  model_spec <- jmv$modelSpec
  return(model_spec)
}

# Function to compile and validate the SEM model
compile_and_validate_model <- function(model_specification, data) {
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

# Function to perform NCA and summarize results
perform_and_summarize_nca <- function(compiled_model, nca_type, nca_variables, jmv) {
  nca_results <- NCA::NCA(compiled_model, nca_type = nca_type, nca_variables = nca_variables)
  jmv$ncaSummary$setContent(summary(nca_results))
  image <- plot(nca_results)
  print(image)
  jmv$ncaPlot$setState(image)
}

# Main function to orchestrate the NCA-SEM process
main <- function(jmv) {
  data <- load_and_validate_data(jmv)
  model_specification <- define_model_specification(jmv)
  compiled_model <- compile_and_validate_model(model_specification, data)
  nca_type <- jmv$ncaType
  nca_variables <- c(jmv$ncaVariables)
  perform_and_summarize_nca(compiled_model, nca_type, nca_variables, jmv)
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

perform_nca_sem <- function(compiled_model, data) {
  if (is.null(compiled_model)) {
    stop("Modelo SEM não compilado. Execute a análise SEM primeiro.")
  }
  tryCatch(
    {
      nca_results <- NCA::NCA(compiled_model)
      return(nca_results)
    },
    error = function(e) {
      message("Erro durante a análise NCA: ", e$message)
      return(NULL)
    }
  )
}