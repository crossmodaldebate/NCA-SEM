generate_sem_outputs <- function(compiled_model, jmv) {
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
    fade = FALSE
  )

  print(image)
  jmv$semPlot$setState(image)
}