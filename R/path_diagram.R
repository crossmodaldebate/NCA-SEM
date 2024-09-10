# Load necessary libraries
library(lavaan)
library(semPlot)
library(jamovi)

# Function to generate a path diagram for a compiled SEM model
path_diagram <- function(model_compiled, jmv) {
  image <- semPlot::semPaths(model_compiled,
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