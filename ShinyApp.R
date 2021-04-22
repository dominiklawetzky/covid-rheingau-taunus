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
                           h3("Infektionsverlauf"),
                           plotOutput("cases",
                                      width = "100%"),
                           h3("Vier-Wochen-Vergleich"),
                           p("verwendet das Ende des Betrachtungszeitraums als Endzeitpunkt"),
                           plotOutput("comp",
                                      width = "100%")),
                  
                  tabPanel("Altersstruktur", 
                           plotOutput("age",
                                      width = "100%"),
                           
                           br(),
                           
                           plotOutput("age_line"),
                  
                           br(),
                  
                           plotOutput("age_line2")),
                            
                  
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
    labs(color = "Legende") +
    scale_color_manual(name = "Legende",
                       values = c("Kumulierte Fallzahl" = "#E69F00", "Neue Fälle" = "#56B4E9")) +
    theme_light() +
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
      theme_light()+
      scale_x_date(limits = c(min, max))
  
  if(input$smooth == 1){print(plot1_gl)}
    else {print(plot1)}
    
   })


# Altersstatistik 1

output$age <- renderPlot({

  
  min <- as.Date(input$date[1], "%Y-%m-%d")
  max <- as.Date(input$date[2], "%Y-%m-%d")
  
  altersdaten1 <- filter(agg_alter, date >= as.Date(min) & date <= as.Date(max))
  
  plot2 = ggplot(data = altersdaten1, 
                 aes(x = date, y = cases, fill = as.factor(age))) +
    geom_bar(stat = "identity") +
    labs(title = "Altersstruktur der täglichen Neuinfektionen",
         subtitle = "Rheingau-Taunus-Kreis",
         caption = "COVID-Panel Rheingau-Taunus") +
    scale_fill_discrete(name = "Altersgruppe") +
    theme_light()+
    xlab("Meldedatum") +
    ylab("Neue Fälle")
  
  print(plot2)
  
})
    


# Altersstatistik 2

output$age_line <- renderPlot({
  
  
  min <- as.Date(input$date[1], "%Y-%m-%d")
  max <- as.Date(input$date[2], "%Y-%m-%d")
  
  agg_alter3 <- filter(agg_alter3, Group.1 >= as.Date(min) & Group.1 <= as.Date(max))
  # aggregate.data.frame(cbind(age, cases), data = agg_alter, FUN = sum)
  
  plot3 = ggplot(data = agg_alter3,
                 aes(x = Group.1, y = x, color = as.factor(Group.2))) +
    geom_line() +
    labs(title = "Tägliche Neuinfektionen nach Alter",
         subtitle = "Rheingau-Taunus-Kreis",
         caption = "COVID-Panel Rheingau-Taunus") +
    scale_color_discrete(name = "Altersgruppe") +
    theme_light() +
    xlab("Meldedatum") +
    ylab("Neue Fälle")
  
  print(plot3)
  
})

# Altersstatistik 3

output$age_line2 <- renderPlot({
  
  min <- as.Date(input$date[1], "%Y-%m-%d")
  max <- as.Date(input$date[2], "%Y-%m-%d")
  
 agg_alter3 <- filter(agg_alter3, Group.1 >= as.Date(min) & Group.1 <= as.Date(max))

plot4 = ggplot(data = agg_alter3,
               aes(x = Group.1, y = x)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Tägliche Neuinfektionen nach Alter mit LOESS-Glättung",
       subtitle = "Rheingau-Taunus-Kreis",
       caption = "COVID-Panel Rheingau-Taunus") +
  scale_color_discrete(name = "Altersgruppe") +
  theme_light() +
  xlab("Meldedatum") +
  ylab("Neue Fälle") +
  facet_wrap(~Group.2)

print(plot4)

})



# Todesfälle

output$deaths <- renderPlot({
  
  coeff <- input$scale
  
  min <- as.Date(input$date[1], "%Y-%m-%d")
  max <- as.Date(input$date[2], "%Y-%m-%d")
  
  
  plot5 <- ggplot() +
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
  
  plot5_gl <- ggplot() +
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

  
  if(input$smooth == 1){print(plot5_gl)}
  else {print(plot5)}
  
})
  
# Sieben-Tages-Vergleich
  
  output$comp <- renderPlot({
    
  blue <- "#56B4E9"
  yellow <- "#E69F00"
    
  today <- as.Date(input$date[2], "%Y-%m-%d")
  sevendays <- as.Date(input$date[2]-7, "%Y-%m-%d")
  fourteendays <- as.Date(input$date[2]-14, "%Y-%m-%d")
  twentyonedays <- as.Date(input$date[2]-21, "%Y-%m-%d")
  twentyeightdays <- as.Date(input$date[2]-28, "%Y-%m-%d")
  
  agg_faelle$Date <- as.Date(agg_faelle$Date)
  currentweek_sum <- sum(filter(agg_faelle, Date >= as.Date(sevendays), Date <= as.Date(today))$NewCases)
  lastweek_sum <- sum(filter(agg_faelle, Date >= as.Date(fourteendays), Date <= as.Date(sevendays))$NewCases)
  
  
  currentweek <- filter(agg_faelle, Date >= as.Date(sevendays), Date <= as.Date(today))
  
  lastweek <- filter(agg_faelle, Date >= as.Date(fourteendays), Date < as.Date(sevendays))
  
  lastlastweek <- filter(agg_faelle, Date >= as.Date(twentyonedays), Date < as.Date(fourteendays))
  
  lastlastlastweek <- filter(agg_faelle, Date >= as.Date(twentyeightdays), Date < as.Date(twentyonedays))
  
  together <- c(currentweek$NewCases, lastweek$NewCases, lastlastweek$NewCases, lastlastlastweek$NewCases)
  together_mean <- mean(together)
  
  layout.matrix <- matrix(c(1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12), ncol = 4, nrow = 3)
  layout(mat = layout.matrix,
         heights = c(5.5, 3, 3), # Heights of the two rows
         widths = c(1)) # Widths of the two columns
  layout.show(12)

  
  boxplot(lastlastlastweek$NewCases,
          main = paste(twentyeightdays, "bis", twentyonedays),
          ylab = "Gemeldete Fälle / Tag",
          ylim = c(min(together), max(together)+5))
  boxplot(lastlastweek$NewCases,
          main = paste(twentyonedays, "bis", fourteendays),
          ylab = "Gemeldete Fälle / Tag",
          ylim = c(min(together), max(together)+5))
  boxplot(lastweek$NewCases,
          main = paste(fourteendays, "bis", sevendays),
          ylab = "Gemeldete Fälle / Tag",
          ylim = c(min(together), max(together)+5))
  boxplot(currentweek$NewCases,
          main = paste(sevendays, "bis", today),
          ylab = "Gemeldete Fälle / Tag",
          ylim = c(min(together), max(together)+5))
  
  boxplot(mean(lastlastlastweek$NewCases),
          horizontal = FALSE,
          main = "Sieben-Tage-Mittelwert",
          ylab = "Gemeldete Fälle / Tag",
          ylim = c(together_mean-(together_mean/2),together_mean+(together_mean/2)))
  boxplot(mean(lastlastweek$NewCases),
          horizontal = FALSE,
          main = "Sieben-Tage-Mittelwert",
          ylab = "Gemeldete Fälle / Tag",
          ylim = c(together_mean-(together_mean/2),together_mean+(together_mean/2)))
  boxplot(mean(lastweek$NewCases),
          horizontal = FALSE,
          main = "Sieben-Tage-Mittelwert",
          ylab = "Gemeldete Fälle / Tag",
          ylim = c(together_mean-(together_mean/2),together_mean+(together_mean/2)))
  boxplot(mean(currentweek$NewCases),
          horizontal = FALSE,
          main = "Sieben-Tage-Mittelwert",
          ylab = "Gemeldete Fälle / Tag",
          ylim = c(together_mean-(together_mean/2),together_mean+(together_mean/2)))
  
  plot(lastlastlastweek$NewCases,
       type = "h",
       main = "Sieben-Tage-Verlauf",
       ylab = "Gemeldete Fälle / Tag",
       xlab = "Tage",
       ylim = c(min(lastlastlastweek$NewCases), max(lastlastlastweek$NewCases)+5),
       col = "black")
  plot(lastlastweek$NewCases,
       type = "h",
       main = "Sieben-Tage-Verlauf",
       ylab = "Gemeldete Fälle / Tag",
       xlab = "Tage",
       ylim = c(min(lastlastweek$NewCases), max(lastlastweek$NewCases)+5),
       col = "black")
  plot(lastweek$NewCases,
       type = "h",
       main = "Sieben-Tage-Verlauf",
       ylab = "Gemeldete Fälle / Tag",
       xlab = "Tage",
       ylim = c(min(lastweek$NewCases), max(lastweek$NewCases)+5),
       col = "black")
  plot(currentweek$NewCases,
       type = "h",
       main = "Sieben-Tage-Verlauf",
       ylab = "Gemeldete Fälle / Tag",
       xlab = "Tage",
       ylim = c(min(currentweek$NewCases), max(currentweek$NewCases)+5),
       col = "black")
  

})


}




##### Applikation starten -----
shinyApp(ui = ui, server = server)


