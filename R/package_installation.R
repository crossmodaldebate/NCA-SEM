# Lista de pacotes necessários
required_packages <- c("lavaan", "NCA", "semPlot", "dplyr", "tidyr", "matrixcalc")

# Verificar se os pacotes estão instalados e instalá-los, se necessário
for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

# Carregar os pacotes
library(lavaan)
library(NCA)
library(semPlot)
library(dplyr)
library(tidyr)
library(matrixcalc)

define_model_specification <- function() {
  '
    Motivation =~ Mot1 + Mot2 + Mot3
    Performance =~ Des1 + Des2 + Des3
    Performance ~ Motivation
  '
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

compile_model <- function(model_specification, data) {
  observed_vars <- c("Mot1", "Mot2", "Mot3", "Des1", "Des2", "Des3")
  missing_vars <- observed_vars[!observed_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop("Variáveis observadas ausentes no conjunto de dados: ", paste(missing_vars, collapse = ", "))
  }
  data_scaled <- scale(data)
  cov_matrix <- check_and_adjust_covariance(data_scaled)
  compiled_model <- tryCatch({
    lavaan::sem(model_specification, sample.cov = cov_matrix, sample.nobs = nrow(data_scaled), fixed.x = FALSE, control = list(maxiter = 1000, trace = 6)) 
  }, error = function(e) {
    message("Erro durante a compilação do modelo: ", e$message)
    return(NULL)
  })
  compiled_model
}

perform_nca_sem <- function(compiled_model, data) {
  if (is.null(compiled_model)) {
    stop("Modelo SEM não compilado. Execute a análise SEM primeiro.")
  }
  tryCatch({
    nca_results <- NCA::NCA(compiled_model)
    return(nca_results)
  }, error = function(e) {
    message("Erro durante a análise NCA: ", e$message)
    return(NULL)
  })
}

plot_nca_sem <- function(compiled_model) {
  semPlot::semPaths(compiled_model, 
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
}

data <- read.csv("read.csv")

data <- data %>%
  separate(col = "Mot1.Mot2.Mot3.Des1.Des2.Des3", into = c("Mot1", "Mot2", "Mot3", "Des1", "Des2", "Des3"), sep = ";") %>%
  mutate(across(c("Mot1", "Mot2", "Mot3", "Des1", "Des2", "Des3"), as.numeric))

model_specification <- define_model_specification()
compiled_model <- compile_model(model_specification, data)

if (!is.null(compiled_model)) {
  nca_results <- perform_nca_sem(compiled_model, data)
  print(nca_results)
  plot_nca_sem(compiled_model)
}