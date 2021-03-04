##### Preamble -----

# Working Directory
setwd("/Users/dominiklawetzky/Documents/GitHub/covid-rheingau-taunus")

# Package names
packages <- c("ggplot2", "readxl", "dplyr", "tidyr", "knitr", "car", "psych", "lmtest", "ggpubr", "ggstatsplot", "jsonlite", "pander", "abind", "RColorBrewer", "rococo", "shiny")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
