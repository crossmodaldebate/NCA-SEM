perform_nca <- function(compiled_model, nca_type, nca_variables, jmv) {
  nca_results <- NCA::NCA(compiled_model, nca_type = nca_type, nca_variables = nca_variables)

  jmv$ncaSummary$setContent(summary(nca_results))

  image <- plot(nca_results)
  print(image)
  jmv$ncaPlot$setState(image)
}