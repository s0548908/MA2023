# Shapley Values Masterarbeit

**Autor:** Stephan Baartz  
**Datum:** 2023-09-17

## Inhaltsverzeichnis
1. [Shapley Values Masterarbeit](#shapley-values-masterarbeit)
2. [R Version](#r-version)
3. [R-Studio Version](#r-studio-version)
4. [Packages](#packages)


## Shapley Values Masterarbeit
Diese Arbeit untersucht die Anwendung von Shapley Values – einem Konzept aus der kooperativen Spieltheorie
– zur Interpretation und Erklärung von Vorhersagen dreier gängiger maschineller Lernmodelle: Logistische
Regression, XGBoost und Neuronale Netze mittels Keras/TensorFlow. Die Analyse der Shapley Values
dieser Modelle vermittelt ein tieferes Verständnis der jeweiligen Vorhersageentscheidungen und bietet eine
vergleichende Perspektive hinsichtlich ihrer Erklärbarkeit.

## R und RStudio Versionen

In diesem Projekt wurden die folgenden Versionen von R und RStudio verwendet:

### R Version

- **Version:** 4.2.2
- **Veröffentlichungsdatum:** 31. Oktober 2022
- **Codename:** ucrt
- **Kompatibilität:** Diese Version von R ist kompatibel mit einer Vielzahl von Paketen und Bibliotheken, was eine reibungslose Ausführung des Codes ermöglicht.
- **Download Link:** [R 4.2.2](https://cran.r-project.org/src/base/R-4/R-4.2.2.tar.gz) 

### RStudio Version

- **Version:** 2023.06.1+524
- **Veröffentlichungsdatum:** Juni 2023
- **Features:** Diese Version von RStudio bietet eine Reihe von verbesserten Features, die eine effiziente Code-Entwicklung und -Ausführung ermöglichen, darunter verbesserte Autokomplettierung, Fehlerbehebung und eine benutzerfreundliche Oberfläche.
- **Download Link:** [RStudio 2023.06.1+524](https://www.rstudio.com/products/rstudio/download/) 

Es wird empfohlen, die Modellierung und die App mit den oben genannten Versionen oder neueren Versionen zu verwenden, um eine optimale Funktionalität zu gewährleisten.

## Packages

Die folgende Tabelle listet die in diesem Projekt verwendeten R-Pakete und ihre jeweiligen Versionen auf. Diese Pakete sind notwendig, um die Daten zu reproduzieren und die Shiny App ordnungsgemäß auszuführen. Im Ordner **Daten** finden Sie zwei Dateien (*MA23.rda* und *xgbtmp2.model*), die alle notwendigen Daten enthalten, falls es bei der Reproduktion zu Problemen kommen sollte.

| Package      | Version  | Beschreibung |
|--------------|----------|--------------|
| fastDummies  | 1.7.3    | Ein Paket zur effizienten Erstellung von Dummy-Variablen. |
| tidyverse    | 2.0.0    | Eine Sammlung von R-Paketen, die das Datenmanagement und die Datenanalyse erleichtern. |
| kableExtra   | 1.3.4    | Bietet zusätzliche Funktionen zur Anpassung von Tabellen. |
| visNetwork   | 2.1.2    | Ermöglicht die Visualisierung von Netzwerkgraphen und komplexen Netzwerkstrukturen. |
| keras        | 2.13.0   | Eine Schnittstelle für die Keras Deep Learning Bibliothek. |
| tensorflow   | 2.13.0   | Ein Paket zur Nutzung der TensorFlow Machine Learning Plattform in R. |
| gridExtra    | 2.3      | Ermöglicht die Anordnung von ggplot2, lattice, tableGrob und anderen Grid-basierten Plots in einer anpassbaren Grid-Layout. |
| ggplot2      | 3.4.3    | Ein System zur Erstellung von grafisch ansprechenden Datenvisualisierungen. |
| xgboost      | 1.7.5.1  | Ein effizientes und skalierbares Implementierung von Gradient Boosting. |
| innsight     | 1.7.5.1  | Analyse der neuronalen Netze mittels Deeplift |

Stellen Sie sicher, dass Sie alle diese Pakete installiert und geladen haben, bevor Sie versuchen, die App oder die Skripte in diesem Repository auszuführen. Die Installation kann mit dem Befehl `install.packages("package_name")` durchgeführt werden, wobei "package_name" durch den Namen des jeweiligen Pakets ersetzt wird.


## Kontakt

Bei Problemen, Fragen oder Anregungen zur Modellierung oder zur Shiny App können Sie sich gerne an mich wenden. Ich bin stets bemüht, Ihre Anfragen zeitnah zu beantworten und bei der Lösung eventuell auftretender Probleme zu helfen.

Sie erreichen mich unter folgender E-Mail-Adresse:

- **E-Mail:** baartz@tuta.io

Bitte geben Sie in Ihrer Nachricht eine klare Beschreibung des Problems oder Ihrer Frage an, um eine schnelle und effektive Hilfe zu gewährleisten. Wenn es sich um ein technisches Problem handelt, fügen Sie bitte auch Informationen zu Ihrem Betriebssystem und den Versionen hinzu.



