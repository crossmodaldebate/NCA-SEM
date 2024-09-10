# Load the required testing library
library(testthat)
library(lavaan)
library(NCA)
library(jamovi)

# Test for the load_and_validate_data function
test_that("load_and_validate_data function works correctly", {
  data <- data.frame(Mot1 = c(1, 2, 3), 
                    Mot2 = c(4, 5, 6),
                    Mot3 = c(7, 8, 9),
                    Des1 = c(10, 11, 12),
                    Des2 = c(13, 14, 15),
                    Des3 = c(16, 17, 18))
  
  jmv <- list(data = data)
  
  data_loaded <- load_and_validate_data(jmv)
  
  # Check that the data has the correct columns
  expect_true(all(c("Mot1", "Mot2", "Mot3", "Des1", "Des2", "Des3") %in% colnames(data_loaded)))
})

# Test for the define_model_specification function
test_that("define_model_specification returns correct model specification", {
  jmv <- list(modelSpec = '
    Motivation =~ Mot1 + Mot2 + Mot3
    Performance =~ Des1 + Des2 + Des3
    Performance ~ Motivation
  ')
  
  model_spec <- define_model_specification(jmv)
  
  # Check that the model specification contains the correct variables and structure
  expect_true(grepl("Motivation =~ Mot1 \\+ Mot2 \\+ Mot3", model_spec))
  expect_true(grepl("Performance =~ Des1 \\+ Des2 \\+ Des3", model_spec))
  expect_true(grepl("Performance ~ Motivation", model_spec))
})

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

# Test for the SEM model compilation and validation
test_that("SEM model compiles and validates correctly", {
  data <- data.frame(Mot1 = c(1, 2, 3), 
                    Mot2 = c(4, 5, 6),
                    Mot3 = c(7, 8, 9),
                    Des1 = c(10, 11, 12),
                    Des2 = c(13, 14, 15),
                    Des3 = c(16, 17, 18))
  
  jmv <- list(data = data,
              modelSpec = '
                Motivation =~ Mot1 + Mot2 + Mot3
                Performance =~ Des1 + Des2 + Des3
                Performance ~ Motivation
              ')
  
  model_spec <- define_model_specification(jmv)
  
  compiled_model <- compile_and_validate_model(model_spec, data)
  
  # Check that the compiled model is of the correct class
  expect_true(inherits(compiled_model, "lavaan"))
})

# Test for the NCA process
test_that("NCA process executes correctly", {
  data <- data.frame(Mot1 = c(1, 2, 3), 
                    Mot2 = c(4, 5, 6),
                    Mot3 = c(7, 8, 9),
                    Des1 = c(10, 11, 12),
                    Des2 = c(13, 14, 15),
                    Des3 = c(16, 17, 18))
  
  jmv <- list(data = data,
              modelSpec = '
                Motivation =~ Mot1 + Mot2 + Mot3
                Performance =~ Des1 + Des2 + Des3
                Performance ~ Motivation
              ')
  
  model_spec <- define_model_specification(jmv)
  compiled_model <- compile_and_validate_model(model_spec, data)
  
  nca_type <- "necessity"
  nca_variables <- c("Motivation", "Performance")
  nca_results <- perform_nca(compiled_model, nca_type, nca_variables, jmv)
  
  # Check that the NCA results are of the correct class
  expect_true(inherits(nca_results, "NCA"))
})

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
  
  model_fit <- tryCatch({
    lavaan::sem(model_specification, sample.cov = cov_matrix, sample.nobs = nrow(data_scaled), start = start_values, fixed.x = FALSE, control = list(maxiter = 1000, trace = 6)) 
  }, error = function(e) {
    message("Erro durante a compilação do modelo: ", e$message)
    return(NULL)
  }, warning = function(w) {
    message("Aviso durante a compilação do modelo: ", w$message)
    return(NULL)
  })
  
  if (!inherits(model_fit, "lavaan")) {
    stop("Model did not converge. Please check the model specification and data.")
  }
  return(model_fit)
}

# Function to perform NCA
perform_nca <- function(compiled_model, nca_type, nca_variables, jmv) {
  nca_results <- NCA::NCA(compiled_model, nca_type = nca_type, nca_variables = nca_variables)
  
  jmv$ncaSummary$setContent(summary(nca_results))
  
  image <- plot(nca_results)
  print(image)
  jmv$ncaPlot$setState(image)
}