##### Preamble -----

# Working Directory
#setwd("/Users/dominiklawetzky/Documents/GitHub/covid-rheingau-taunus")

## PACKAGE NAMEN
packages <- c("ggplot2", "readxl", "dplyr", "tidyr","psych", "tidyverse", "ggpubr", "ggstatsplot", "jsonlite", "RColorBrewer","shiny")



## PACKETE INSTALLIEREN, WENN NICHT INSTALLIERT
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}



## PAKETE LADEN
invisible(lapply(packages, library, character.only = TRUE))
