
wlcm <- paste0("\n",
               "--------------------------------------------------------------------------------\n",
               "Welcome to viabilitymetrics version ", utils::packageDescription("viabilitymetrics")$Version, "\n",
               "\n", "\n",
               "To know how to use this package type:", "\n",
               "browseVignettes(package = 'viabilitymetrics')", "\n", " for the package vignette.", "\n",
               "\n",
               "To know whats new in this version type:", "\n",
               "news(package='viabilitymetrics') for the NEWS file.", "\n",
               "\n",
               "To cite the methods in the package type:", "\n",
               "citation(package='viabilitymetrics')", "\n",
               "\n",
               "To suppress this message use:", "\n",
               "suppressPackageStartupMessages(library(viabilitymetrics))", "\n",
               "--------------------------------------------------------------------------------\n")

.onAttach <- function(lib, pkg, ...){
  packageStartupMessage(wlcm)

}
