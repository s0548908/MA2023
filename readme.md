# Shiny App

**Autor:** Stephan Baartz  
**Datum:** 2023-09-17

## Inhaltsverzeichnis
1. [Packages](#packages)
2. [Daten](#daten)

## Packages

Die verwendendeten Packages und ihre Versionen:

| Package             | Version      |
|---------------------|--------------|
| shiny              | 1.7.5        |
| shinydashboard     | 0.7.2        |
| shinydashboardPlus | 2.0.4.9000   |
| dplyr              | 1.1.2        |
| DT                 | 0.28         |
| htmltools          | 0.5.6        |
| xgboost            | 1.7.5.1      |
| plotly             | 4.10.2       |
| shinyWidgets       | 0.7.6        |
| shinyalert         | 3.0.0        |
| kableExtra         | 1.3.4        |
| visNetwork         | 2.1.2        |
| SHAPforxgboost     | 0.1.3        |
| shinymaterial      | 1.2.0        |

## Daten
In dem Ordner Daten befinden sich zwei Dateien *MA23.rda* und *xgbtmp2.model*. Vor dem Start der App ist es wichtig, dass die Daten geladen wurden.

```{r}
xgb_mod.s<-xgb.load("..data/xgbtmp2.model")
load("..data/MA23.rda")
```
