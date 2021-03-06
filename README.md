# Anliegen
Auf dem Weg zu einer [Shiny-App](https://shiny.rstudio.com), um die relevanten Statistiken zum Pandemie-Verlauf übersichtlich zugänglich zu machen. Bis zuletzt stellt der Kreis seine Daten nur als täglich aktualisiertes PDF-Dokument zur Verfügung. Detaillierte Statistiken zu Alter oder Geschlecht sucht man vergeblich.

**Eine vorbildliche Dokumentation folgt, sobald ich Zeit dafür finde.**

Der Code ist *free and open*. 
Wer mithelfen will, kann eine *Pull request* einreichen, oder mich unter [info(at)dominiklawetzky(dot)de](mailto:info@dominiklawetzky.de) kontaktieren.

[**Zur ersten funktionstüchtigen Version**](https://dominiklawetzky.shinyapps.io/covid-rheingau-taunus/)

# Versionhistorie

Vier-Wochen-Vergleich ist noch nicht _live_. Bisher nur auf Desktops sinnvoll nutzbar.

## v0.2 (Beta)
- Daten laden automatisch über die ArcGIS-Schnittstelle des RKI
- Daten werden effizienter geladen (nur Rheingau-Taunus-Kreis)
- ggplot-Implementierung (neue Fälle, kumulierte Fälle, Alterstruktur, Todesfälle)
- geglätte Plots (verschiedene Glättungsmethoden)
- Regler zur Auswahl von Betrachtungszeitraum und Skalierung der y-Achse bei neuen Fällen
- einige Erklärungen hinzugefügt (RKI-Schnittstelle, Download der Plots)

**Was noch kommt:** Anzeige des Betrachtungszeitraums in den Plots, einfacher Download-Button, viele weitere mögliche Plots (z. B. Geschlechterverteilung), Möglichkeit zum Vergleich zweier Betrachtungszeiträume
