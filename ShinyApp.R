##### Preamble -----

# Working Directory
setwd("/Users/dominiklawetzky/Documents/GitHub/covid-rheingau-taunus")

# Package names
packages <- c("ggplot2", "readxl", "dplyr", "tidyr", "knitr", "car", "psych", "lmtest", "ggpubr", "ggstatsplot", "jsonlite", "pander", "abind", "RColorBrewer", "rococo", "COVID19", "shiny", "plotly")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

##### UI ------
ui <- fluidPage(
  titlePanel("COVID-Panel Rheingau-Taunus"),
  
  sidebarLayout(
    sidebarPanel(
      h2("Filter"),
      selectInput("type", label = "Daten", choices = c("confirmed", "tests", "recovered", "deaths")),
      dateRangeInput("date", label = "Datum", start = "2020-01-01"),
    ),
    
    mainPanel(
      h2("Plots"),
      plotlyOutput("covid19plot")
    )
  )
  
)


##### Server Logic ------
server <- function(input, output) {

output$covid19plot <- renderPlotly({
    x <- subset(covid19(country = "DEU", 
                         level = 3, 
                         start = input$date[1], 
                         end = input$date[2]), 
                 administrative_area_level_3 == "LK Rheingau-Taunus-Kreis")
    plot_ly(x = x[["date"]], y = x[[input$type]])
    
   })
      
      }


##### Applikation starten -----
shinyApp(ui = ui, server = server)
