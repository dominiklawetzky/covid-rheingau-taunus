# source("Preamble.R", local = TRUE)

source("Data.R", local = TRUE)

library(shiny)
library(ggplot2)


##### UI ------

ui <- fluidPage(
  titlePanel("COVID-Panel Rheingau-Taunus"),
  
  sidebarLayout(
    sidebarPanel(
      h2("Filter"),
      dateRangeInput("date", label = "Betrachtungszeitraum", start = "2020-01-01"),
      sliderInput("scale", label = "Slakierung (Neue Fälle)", min = .01, max = 1, value = .05),
      p("Die Daten werden mindestens täglich über die API des Robert-Koch-Instituts abgerufen."),
      tags$a(href="https://github.com/dominiklawetzky/covid-rheingau-taunus", 
               "COVID-Panel Rheingau-Taunus, v0.2"),
      tags$div(
        "Gepflegt von",
        tags$a(href="https://dominiklawetzky.de", 
               "Dominik Lawetzky")),
    ),
  
    mainPanel(
      h2("Plots"),
      plotOutput("cases"),
      plotOutput("age"),
      p("Die Plots können über einen Rechtsklick mittels \"Speichern unter\" und ggf. unter Hinzufügen des Suffix \".png\" gespeichert werden."),
    )
  
)
)





##### Server Logic ------
server <- function(input, output, session) {

# Fallzahlen
  
output$cases <- renderPlot({


    coeff <- input$scale
    
    min <- as.Date(input$date[1], "%Y-%m-%d")
    max <- as.Date(input$date[2], "%Y-%m-%d")
  
  
    plot1 <- ggplot() +
    geom_line(data = kum_faelle, 
              aes(x = Date, y = CumCases, color = "Kumulierte Fallzahl"),
              size = 1) +
    geom_line(data = agg_faelle, 
              aes(x = Date, y = NewCases / coeff, color = "Neue Fälle"),
              size = 1) +
    scale_y_continuous(name = "Kumuliert",
                       sec.axis = sec_axis(~.*coeff, name = "Neue Fälle")) +
    labs(title = "Verlauf der Sars-CoV-2-Infektionen",
         subtitle = "Rheingau-Taunus-Kreis",
         caption = "COVID-Panel Rheingau-Taunus",
         color = "Legende") +
    scale_color_manual(name = "Legende",
                       values = c("Kumulierte Fallzahl" = "steelblue", "Neue Fälle" = "pink")) +
    theme(plot.title = element_text(color = "black", face = "bold", size = 20, hjust = .5),
          plot.subtitle = element_text(color = "darkgrey", size = 14, hjust = .5),
          axis.text.x = element_text(vjust = .5, hjust = .5),
          axis.title.x = element_text(vjust = -3),
          axis.title.y.left = element_text(vjust = 2),
          axis.title.y.right = element_text(vjust = 2),
          legend.position = "bottom") +
  scale_x_date(limits = c(min, max))
  
  print(plot1)
    
   })



# Altersstatistik

output$age <- renderPlot({
  
  min <- as.Date(input$date[1], "%Y-%m-%d")
  max <- as.Date(input$date[2], "%Y-%m-%d")
  
  altersdaten <- filter(agg_alter_tab, date >= as.Date(min), date <= as.Date(max))
  
  plot2 = ggplot(data = altersdaten, 
                 aes(x = age, y = Freq)) +
    geom_bar(stat="identity", fill = "steelblue") +
    labs(title = "Alterstruktur der Sars-CoV-2-Infektionen",
         subtitle = "Rheingau-Taunus-Kreis",
         caption = "COVID-Panel Rheingau-Taunus") +
    theme(plot.title = element_text(color = "black", face = "bold", size = 20, hjust = .5),
          plot.subtitle = element_text(color = "darkgrey", size = 14, hjust = .5),
          axis.text.x = element_text(vjust = .5, hjust = .5),
          axis.title.x = element_text(vjust = -3),
          axis.title.y.left = element_text(vjust = 2),
          axis.title.y.right = element_text(vjust = 2)) +
    xlab("Altersgruppen") +
    ylab("absolute Häufigkeit")
  
  print(plot2)
  
})
    
}



##### Applikation starten -----
shinyApp(ui = ui, server = server)


