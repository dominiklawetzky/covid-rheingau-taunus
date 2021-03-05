# source("Preamble.R", local = TRUE)

source("Data.R", local = TRUE)

library(shiny)
library(ggplot2)
library(dplyr)
library(scales)


##### UI ------

ui <- fluidPage(
  titlePanel("COVID-Panel Rheingau-Taunus"),
  
  sidebarLayout(
    sidebarPanel(
      h2("Filter"),
      dateRangeInput("date", label = "Betrachtungszeitraum", start = "2020-01-01"),
      sliderInput("scale", label = "Slakierung (Neue Fälle)", min = .01, max = 1, value = .05),
      
      br(),
      
      checkboxInput("smooth", "Geglättet"),
      conditionalPanel(
        condition = "input.smooth == true",
        selectInput("smoothMethod", "Methode",
                    list("lm", "glm", "gam", "loess")),
        p("Bitte wählen Sie eine Glättungsmethode. Einige Methoden sind nur für bestimmte Betrachtungszeiträume sinnvoll."),
        HTML("<p><b>lm:</b> linear regression model <br> <b>glm:</b> generalized linear model <br> <b>gam:</b> generalized additive mode <br> <b>loess:</b> locally estimated scatterplot smoothing</p>")),
      
      br(),
      
      p("Die Daten werden mindestens täglich über die API des Robert-Koch-Instituts abgerufen."),
      tags$a(href="https://github.com/dominiklawetzky/covid-rheingau-taunus", 
             "COVID-Panel Rheingau-Taunus, v0.2"),
      tags$div(
        "Gepflegt von",
        tags$a(href="https://dominiklawetzky.de", 
               "Dominik Lawetzky"))
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Infektionszahlen", 
                           plotOutput("cases",
                                      width = "100%")),
                  
                  tabPanel("Altersstruktur", 
                           plotOutput("age",
                                      width = "100%"),
                           
                           br(),
                           
                           plotOutput("age_rel",
                                      width = "100%"),
                           br(),
                           
                           tableOutput("age_table")),
                  
                  
                  tabPanel("Todeszahlen", 
                           plotOutput("deaths",
                                      width = "100%")),
                  
                  tabPanel("Info",
                           h2("Plots herunterladen"),
                           p("Die Plots können über einen Rechtsklick mittels \"Speichern unter\" und ggf. unter Hinzufügen des Suffix \".png\" gespeichert werden."),
                           h2("Glättungsmethoden"),
                           HTML("<p>lm: linear regression model <br> glm: generalized linear model <br> gam: generalized additive mode <br> loess: locally estimated scatterplot smoothing</p>"))                           
                  
      )
      
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
    
    
    plot1_gl <- ggplot() +
      geom_smooth(data = kum_faelle, method = input$smoothMethod,
                aes(x = Date, y = CumCases, color = "Kumulierte Fallzahl"),
                size = 1) +
      geom_smooth(data = agg_faelle, method = input$smoothMethod,
                aes(x = Date, y = NewCases / coeff, color = "Neue Fälle"),
                size = 1) +
      scale_y_continuous(name = "Kumuliert",
                         sec.axis = sec_axis(~.*coeff, name = "Neue Fälle")) +
      labs(title = "Verlauf der Sars-CoV-2-Infektionen (geglättet)",
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
  
  if(input$smooth == 1){print(plot1_gl)}
    else {print(plot1)}
    
   })


# Altersstatistik (absolut)

output$age <- renderPlot({

  
  min <- as.Date(input$date[1], "%Y-%m-%d")
  max <- as.Date(input$date[2], "%Y-%m-%d")
  
  altersdaten1 <- filter(agg_alter_tab, date >= as.Date(min), date <= as.Date(max))
  
  plot2 = ggplot(data = altersdaten1, 
                 aes(x = age, y = Freq)) +
    geom_bar(stat="identity", fill = "steelblue") +
    labs(title = "Altersstruktur (absolute Häufigkeit)",
         subtitle = "Sars-CoV-2-Infektionen im Rheingau-Taunus-Kreis",
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
    

# Altersstatistik (relativ)

output$age_rel <- renderPlot({
  
  
  min <- as.Date(input$date[1], "%Y-%m-%d")
  max <- as.Date(input$date[2], "%Y-%m-%d")
  
  altersdaten2 <- filter(agg_alter, date >= as.Date(min), date <= as.Date(max))
  altersdaten2 <- prop.table(table(altersdaten2))
  altersdaten2 <- as.data.frame(altersdaten2)
  
  plot3 = ggplot(data = altersdaten2, 
                 aes(x = age, y = Freq)) +
    geom_bar(stat="identity", fill = "steelblue") +
    labs(title = "Altersstruktur (relative Häufigkeit)",
         subtitle = "Sars-CoV-2-Infektionen im Rheingau-Taunus-Kreis",
         caption = "COVID-Panel Rheingau-Taunus") +
    theme(plot.title = element_text(color = "black", face = "bold", size = 20, hjust = .5),
          plot.subtitle = element_text(color = "darkgrey", size = 14, hjust = .5),
          axis.text.x = element_text(vjust = .5, hjust = .5),
          axis.title.x = element_text(vjust = -3),
          axis.title.y.left = element_text(vjust = 2),
          axis.title.y.right = element_text(vjust = 2)) +
    xlab("Altersgruppen") +
    ylab("relative Häufigkeit")
  
  print(plot3)
  
})


# Tabelle

output$age_table <- renderTable({
  
  min <- as.Date(input$date[1], "%Y-%m-%d")
  max <- as.Date(input$date[2], "%Y-%m-%d")
  
  altersdaten3 <- filter(agg_alter, date >= as.Date(min), date <= as.Date(max))
  altersdaten3 <- prop.table(table(altersdaten3))
  altersdaten3 <- as.data.frame(altersdaten3)
  altersdaten3 <- aggregate.data.frame(altersdaten3$Freq, by = list(altersdaten3$age), FUN = sum)
  altersdaten3$x <-label_percent()(altersdaten3$x)
  colnames(altersdaten3) <- c("Altersgruppe","Prozentuale Häufigkeit")
  print(altersdaten3)
  

  
})


# Todesfälle

output$deaths <- renderPlot({
  
  coeff <- input$scale
  
  min <- as.Date(input$date[1], "%Y-%m-%d")
  max <- as.Date(input$date[2], "%Y-%m-%d")
  
  
  plot3 <- ggplot() +
    geom_line(data = kum_tote, 
              aes(x = Date, y = CumDeaths, color = "Kumulierte Todesfälle"),
              size = 1) +
    geom_line(data = agg_tote, 
              aes(x = Date, y = NewDeaths / coeff, color = "Neue Todesfälle"),
              size = 1) +
    scale_y_continuous(name = "Kumuliert",
                       sec.axis = sec_axis(~.*coeff, name = "Neue Todesfälle")) +
    labs(title = "Todesfälle durch Sars-CoV-2",
         subtitle = "Rheingau-Taunus-Kreis",
         caption = "COVID-Panel Rheingau-Taunus",
         color = "Legende") +
    scale_color_manual(name = "Legende",
                       values = c("Kumulierte Todesfälle" = "steelblue", "Neue Todesfälle" = "pink")) +
    theme(plot.title = element_text(color = "black", face = "bold", size = 20, hjust = .5),
          plot.subtitle = element_text(color = "darkgrey", size = 14, hjust = .5),
          axis.text.x = element_text(vjust = .5, hjust = .5),
          axis.title.x = element_text(vjust = -3),
          axis.title.y.left = element_text(vjust = 2),
          axis.title.y.right = element_text(vjust = 2),
          legend.position = "bottom") +
    scale_x_date(limits = c(min, max))
  
  plot3_gl <- ggplot() +
    geom_smooth(data = kum_tote, method = input$smoothMethod, 
              aes(x = Date, y = CumDeaths, color = "Kumulierte Todesfälle"),
              size = 1) +
    geom_smooth(data = agg_tote, method = input$smoothMethod, 
              aes(x = Date, y = NewDeaths / coeff, color = "Neue Todesfälle"),
              size = 1) +
    scale_y_continuous(name = "Kumuliert",
                       sec.axis = sec_axis(~.*coeff, name = "Neue Todesfälle")) +
    labs(title = "Todesfälle durch Sars-CoV-2 (geglättet)",
         subtitle = "Rheingau-Taunus-Kreis",
         caption = "COVID-Panel Rheingau-Taunus",
         color = "Legende") +
    scale_color_manual(name = "Legende",
                       values = c("Kumulierte Todesfälle" = "steelblue", "Neue Todesfälle" = "pink")) +
    theme(plot.title = element_text(color = "black", face = "bold", size = 20, hjust = .5),
          plot.subtitle = element_text(color = "darkgrey", size = 14, hjust = .5),
          axis.text.x = element_text(vjust = .5, hjust = .5),
          axis.title.x = element_text(vjust = -3),
          axis.title.y.left = element_text(vjust = 2),
          axis.title.y.right = element_text(vjust = 2),
          legend.position = "bottom") +
    scale_x_date(limits = c(min, max))

  
  if(input$smooth == 1){print(plot3_gl)}
  else {print(plot3)}
  
})


}




##### Applikation starten -----
shinyApp(ui = ui, server = server)


