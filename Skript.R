##### Einlesen / Aufbereiten -----

setwd("/Users/dominiklawetzky/Documents/GitHub/covid-rheingau-taunus")

library(ggplot2)
library(dplyr)
library(jsonlite)
library(downloader)
library(RJSONIO)

url <- "https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/RKI_COVID19/FeatureServer/0/query?where=1%3D1&outFields=Bundesland,Landkreis,Altersgruppe,Geschlecht,AnzahlFall,AnzahlTodesfall,Meldedatum,Datenstand,NeuerFall,NeuerTodesfall,NeuGenesen,AnzahlGenesen&outSR=4326&f=json"


options(timeout=200)
download.file(url = url, destfile = "Data.json")

# Versuch 1

json_data <- jsonlite::fromJSON("Data.json")

json_data <- json_data %>% as_tibble()
glimpse(json_data)

test <- as.data.frame(json_data$features)


# Versuch 2

test <- stream_in(file("Data.json"))
test2 <- as_tibble("Data.json")


# Ohne API

# Download-URL: https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0

covid_data <- read.csv("RKI_COVID19.csv")

covid_hessen <- subset(covid_data, Bundesland=="Hessen")

covid_hessen2 <- subset(test, features.properties.IdBundesland == 6)


covid_hessen_wiesbaden <- subset(covid_data, Landkreis=="SK Wiesbaden")

covid_hessen_rheingau_taunus <- subset(covid_data, Landkreis=="LK Rheingau-Taunus-Kreis")

covid_hessen_rheingau_taunus_FALL <- data.frame(Meldedatum = covid_hessen_rheingau_taunus$Meldedatum, 
                                                Fallzahl = covid_hessen_rheingau_taunus$AnzahlFall)


##### Altersstruktur -----

covid_hessen_rheingau_taunus_ALTER <- data.frame(Altersgruppe = covid_hessen_rheingau_taunus$Altersgruppe, 
                                                 Fallzahl = covid_hessen_rheingau_taunus$AnzahlFall)

covid_hessen_rheingau_taunus_ALTER_AG <- aggregate.data.frame(covid_hessen_rheingau_taunus_ALTER$Fallzahl, 
                                                              list(Altersgruppe=covid_hessen_rheingau_taunus_ALTER$Altersgruppe), FUN = sum)

covid_hessen_rheingau_taunus_ALTER_AG <- data.frame(Altersgruppen = covid_hessen_rheingau_taunus_ALTER_AG$Altersgruppe,
                                                    Gesamtanzahl = covid_hessen_rheingau_taunus_ALTER_AG$x)


barplot(covid_hessen_rheingau_taunus_ALTER_AG$Gesamtanzahl ~ covid_hessen_rheingau_taunus_ALTER_AG$Altersgruppen)

alterstruktur = ggplot(data = covid_hessen_rheingau_taunus_ALTER_AG, 
                       aes(x = Altersgruppen, y = Gesamtanzahl)) +
    geom_bar(stat="identity", fill = "steelblue") +
    labs(title = "Alterstruktur der Sars-CoV-2-Infektionen",
    subtitle = "Rheingau-Taunus-Kreis",
    caption = as.Date(Sys.time())) +
    theme(plot.title = element_text(color = "black", face = "bold", size = 16, hjust = .5),
    plot.subtitle = element_text(color = "darkgrey", size = 12, hjust = .5))

ggsave("alterstruktur.jpg",
       plot = alterstruktur)

alterstruktur

##### Kumulierte Fallzahl -----

covid_hessen_rheingau_taunus_FALL <- data.frame(Meldedatum = as.Date(covid_hessen_rheingau_taunus$Meldedatum), 
                                                Fallzahl = covid_hessen_rheingau_taunus$AnzahlFall)

covid_hessen_rheingau_taunus_FALL_AG <- aggregate.data.frame(covid_hessen_rheingau_taunus_FALL$Fallzahl, 
                                                             list(Meldedatum=covid_hessen_rheingau_taunus_FALL$Meldedatum), FUN = sum)

covid_hessen_rheingau_taunus_FALL_AG <- data.frame(Meldedatum = covid_hessen_rheingau_taunus_FALL_AG$Meldedatum,
                                                   Neu = covid_hessen_rheingau_taunus_FALL_AG$x)
                                                   
covid_hessen_rheingau_taunus_FALL_AG <-  mutate(group_by(covid_hessen_rheingau_taunus_FALL_AG), 
                                                Kumuliert = cumsum(Neu))

coeff <- .05

faelle = ggplot() +
  geom_line(data = covid_hessen_rheingau_taunus_FALL_AG, 
            aes(x = Meldedatum, y = Kumuliert, color = "Kumulierte Fallzahl"),
            size = 1) +
  geom_line(data = covid_hessen_rheingau_taunus_FALL_AG, 
            aes(x = Meldedatum, y = Neu / coeff, color = "Neue Fälle"),
            size = 1) +
  scale_y_continuous(name = "Kumuliert",
                     sec.axis = sec_axis(~.*coeff, name = "Neue Fälle")) +
  labs(title = "Verlauf der Sars-CoV-2-Infektionen",
       subtitle = "Rheingau-Taunus-Kreis",
       caption = as.Date(Sys.time()),
       color = "Legende") +
  scale_color_manual(name = "Legende",
                     values = c("Kumulierte Fallzahl" = "steelblue", "Neue Fälle" = "pink")) +
  theme(plot.title = element_text(color = "black", face = "bold", size = 16, hjust = .5),
        plot.subtitle = element_text(color = "darkgrey", size = 12, hjust = .5),
        axis.text.x = element_text(vjust = .5, hjust = .5),
        axis.title.x = element_text(vjust = -3),
        axis.title.y.left = element_text(vjust = 2),
        axis.title.y.right = element_text(vjust = 2),
        legend.position = "bottom")

min <- as.Date("2020-03-1")
max <- NA

faelle <- faelle + scale_x_date(limits = c(min, max))
faelle

ggsave("faelle.jpg", 
       plot = faelle_abschnitt,
       width = 12,
       height = 5,
       limitsize = FALSE)

##### Fallzahlen geglättet -----

coeff <- .05

faelle_smooth = ggplot() +
  geom_smooth(data = covid_hessen_rheingau_taunus_FALL_AG, 
            aes(x = Meldedatum, y = Kumuliert, color = "Kumulierte Fallzahl, geglättet"),
            size = 1, level = 0) +
  geom_smooth(data = covid_hessen_rheingau_taunus_FALL_AG, 
            aes(x = Meldedatum, y = Neu / coeff, color = "Neue Fälle, geglättet"),
            size = 1, level = 0) +
  scale_y_continuous(name = "Kumuliert",
                     sec.axis = sec_axis(~.*coeff, name = "Neue Fälle, geglättet")) +
  labs(title = "Gelätteter Verlauf der Sars-CoV-2-Infektionen",
       subtitle = "Rheingau-Taunus-Kreis",
       caption = as.Date(Sys.time()),
       color = "Legende") +
  scale_color_manual(name = "Legende",
                     values = c("Kumulierte Fallzahl, geglättet" = "steelblue", "Neue Fälle, geglättet" = "pink")) +
  theme(plot.title = element_text(color = "black", face = "bold", size = 16, hjust = .5),
        plot.subtitle = element_text(color = "darkgrey", size = 12, hjust = .5),
        axis.text.x = element_text(vjust = .5, hjust = .5),
        axis.title.x = element_text(vjust = -3),
        axis.title.y.left = element_text(vjust = 2),
        axis.title.y.right = element_text(vjust = 2),
        legend.position = "bottom")

min <- as.Date("2020-03-1")
max <- NA

faelle_smooth <- faelle_smooth + scale_x_date(limits = c(min, max))
faelle_smooth

ggsave("faelle_smooth.jpg", 
       plot = faelle_smooth,
       width = 12,
       height = 5,
       limitsize = FALSE)


##### Geschlechterstruktur -----

##### Inzidenzwert -----

##### Fallzahlenvergleich mit Wiesbaden -----

covid_hessen_rheingau_taunus_FALL_AG[226,3]
covid_hessen_rheingau_taunus_FALL_AG[281,3]
