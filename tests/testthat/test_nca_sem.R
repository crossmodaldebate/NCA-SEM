test_that("NCA-SEM analysis runs without errors", {
# Arquivo: tests/testthat/test-nca_sem.R

# Carrega as bibliotecas necessárias
library(testthat)
library(lavaan)
library(NCA)
library(jamovi)

# Função para carregar e validar os dados
load_and_validate_data <- function(jmv) {
  data <- jmv$data
  required_vars <- c("Mot1", "Mot2", "Mot3", "Des1", "Des2", "Des3")
  missing_vars <- required_vars[!required_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop("The following required variables are missing from the dataset: ", paste(missing_vars, collapse = ", "))
  }
  return(data)
}

# Função para definir a especificação do modelo SEM
define_model_specification <- function(jmv) {
  return(jmv$modelSpec)
}

# Função para verificar e ajustar a matriz de covariância
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

# Função para gerar valores iniciais para o modelo SEM
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

# Função para compilar e validar o modelo SEM
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

# Função para realizar a análise NCA
perform_nca <- function(compiled_model, nca_type, nca_variables) {
  nca_results <- NCA::NCA(compiled_model, nca_type = nca_type, nca_variables = nca_variables)
  return(nca_results)
}

# Dados de teste
data <- data.frame(Mot1 = c(1, 2, 3), 
                  Mot2 = c(4, 5, 6),
                  Mot3 = c(7, 8, 9),
                  Des1 = c(10, 11, 12),
                  Des2 = c(13, 14, 15),
                  Des3 = c(16, 17, 18))

# Especificação do modelo de teste
model_spec <- '
  Motivation =~ Mot1 + Mot2 + Mot3
  Performance =~ Des1 + Des2 + Des3
  Performance ~ Motivation
'

# Teste para a função load_and_validate_data
test_that("load_and_validate_data function works correctly", {
  jmv <- list(data = data)
  data_loaded <- load_and_validate_data(jmv)
  expect_true(all(c("Mot1", "Mot2", "Mot3", "Des1", "Des2", "Des3") %in% colnames(data_loaded)))
})

# Teste para a função define_model_specification
test_that("define_model_specification returns correct model specification", {
  jmv <- list(modelSpec = model_spec)
  model_spec_returned <- define_model_specification(jmv)
  expect_equal(model_spec_returned, model_spec)
})

# Teste para a função compile_and_validate_model
test_that("SEM model compiles and validates correctly", {
  compiled_model <- compile_and_validate_model(model_spec, data)
  expect_true(inherits(compiled_model, "lavaan"))
})

# Teste para a função perform_nca
test_that("NCA process executes correctly", {
  compiled_model <- compile_and_validate_model(model_spec, data)
  nca_type <- "necessity"
  nca_variables <- c("Motivation", "Performance")
  nca_results <- perform_nca(compiled_model, nca_type, nca_variables)
  expect_true(inherits(nca_results, "NCA"))
})
})
