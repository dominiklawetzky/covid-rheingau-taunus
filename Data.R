rm(list = ls())

##### API-Zugriff -----

url <- "https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/RKI_COVID19/FeatureServer/0/query?where=IdLandkreis%20%3D%20'06439'&outFields=*&outSR=4326&f=json"

download.file(url = url, destfile = "Data.json") # Daten herunterladen


json_data <- jsonlite::fromJSON("Data.json") # Daten einlesen

data_rtk <- as.data.frame(json_data$features) # In Dataframe konvertieren

##### Datum -----

data_rtk$attributes$Meldedatum <- as.POSIXct(data_rtk$attributes$Meldedatum/1000, origin = "1970-01-01", tz = "UTC") # Datumsformatierung korrigieren

data_rtk$attributes$Meldedatum <- as.Date(data_rtk$attributes$Meldedatum, "%Y-%m-%d")

##### Fälle -----

agg_faelle <- aggregate.data.frame(data_rtk$attributes$AnzahlFall, 
                                   by = list(data_rtk$attributes$Meldedatum), 
                                   FUN = sum) # Aggregation der Neuen Fälle

agg_faelle <- data.frame(Date = agg_faelle$Group.1,
                         NewCases = agg_faelle$x) # Variablenbeschriftung hinzufügen

kum_faelle <- data.frame(Date = agg_faelle$Date,
                         CumCases = cumsum(agg_faelle$NewCases)) # Kumulierte Fälle



##### Alter -----

agg_alter <- data.frame(date = data_rtk$attributes$Meldedatum,
                        age = data_rtk$attributes$Altersgruppe,
                        cases = data_rtk$attributes$AnzahlFall)

agg_alter_tab <- table(agg_alter)

agg_alter_tab <- as.data.frame(agg_alter_tab)
agg_alter_tab$date <- as.Date(agg_alter_tab$date)


agg_alter_rel <- prop.table(table(agg_alter))
agg_alter_rel <- as.data.frame(agg_alter_rel)
agg_alter_rel$date <- as.Date(agg_alter_rel$date)

##### Todesfälle -----

agg_tote <- aggregate.data.frame(data_rtk$attributes$AnzahlTodesfall, 
                                   by = list(data_rtk$attributes$Meldedatum), 
                                   FUN = sum) # Aggregation der Neuen Todesfälle

agg_tote <- data.frame(Date = agg_tote$Group.1,
                       NewDeaths = agg_tote$x) # Variablenbeschriftung hinzufügen

kum_tote <- data.frame(Date = agg_tote$Date,
                         CumDeaths = cumsum(agg_tote$NewDeath)) # Kumulierte Todesfälle



