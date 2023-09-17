library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(DT)
library(htmltools)
library(xgboost)
library(plotly)
library(shinyWidgets)
library(shinyalert)
library(kableExtra)
library(visNetwork)
library(SHAPforxgboost)
library(shinymaterial)
#library(caret)

ui<-
  fluidPage(
    withMathJax(),
    includeMathJax = T,
    includeCSS("style.css"),
    column(
      width = 12,
      class = "row-cols-xs-12",
      navbarPage(
        title = 'Baartz Solutions',
        fluid = TRUE,
        collapsible = TRUE,
        tabPanel(
          # TP Home ####
          title = "Home",
          dashboardPage(
            dashboardHeader(
              disable = T
            ),
            dashboardSidebar(
            ),
            
            dashboardBody(
              HTML(
              '<div style="margin-top: 150px; text-align: center;">
                <video autoplay muted playsinline width="70%" height="70%">
                <source src="https://baartz.solutions/start.mp4" type="video/mp4">
                Your browser does not support the video tag.
                </video>
              </div>'
              )
            )
          )
        ),
        tabPanel(
          title = "Masterarbeit 2023",
          dashboardPage(
            dashboardHeader(
              disable = T
            ),
            dashboardSidebar(
              sidebarMenu(
                id = "tabs",
                menuItem(
                  text = "Überblick",
                  tabName = "shap_überblick"
                ),
                menuItem(
                  text = "Datensatz",
                  tabName = "shap_datensatz"
                ),
                menuItem(
                  text = "Trainings- und Testdaten",
                  tabName = "shap_train_test"
                ),
                menuItem(
                  text = "Model Analysen",
                  menuItem(
                    text = "Information",
                    tabName = "fit_info"
                  ),
                  hr(),
                  menuItem(
                    text = "Logistische Reg.",
                    tabName = "fit_lr"
                  ),
                  menuItem(
                    text = "Neuronales Netz",
                    tabName = "fit_nn"
                  ),
                  
                  menuItem(
                    text = "xgBoost",
                    tabName = "fit_xg"
                  )
                ),
                menuItem(
                  text = "Shaple-Values",
                  menuItem(
                    text = "Information",
                    tabName = "shap_info"
                  ),
                  hr(),
                  menuItem(
                    text = "logistische Reg.",
                    tabName = "shap_lr"
                  ),
                  menuItem(
                    text = "Neuronales Netz",
                    tabName = "shap_nn"
                  ),
                  
                  menuItem(
                    text = "xgBoost",
                    tabName = "shap_xg"
                  )
                ),
                menuItem(
                  text = "Kreditsimulation",
                  tabName = "shap_shap"
                )
              )
            ),
            
            dashboardBody(
              # Startet bei Tabwechsel wieder oben
              tags$script(
                HTML(
                  "$(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function (e) {
              window.scrollTo(0, 0);
              });"
                )
              ),
              # TP Überblick ####
              tabItems(
                tabItem(
                  tabName = "shap_überblick",
                  fluidRow(
                    column(2),
                    column(
                      8,
                      tags$ol(
                        tags$li(
                          h3("Datenverständnis"),
                          
                          tags$ul(
                            tags$li(
                              style = "font-size: 18px;",
                              "Zu Beginn wird der Datensatz untersucht, um ein 
                              tiefes Verständnis über die enthaltenen Informationen 
                              zu erhalten."
                            ),
                            tags$li(
                              style = "font-size: 18px;",
                              "Es werden Auffälligkeiten wie Ausreißer geprüft, 
                              um sicherzustellen, dass die Daten qualitativ 
                              hochwertig sind."
                            ),
                            tags$li(
                              style = "font-size: 18px;",
                              "Falls keine signifikanten Auffälligkeiten 
                              festgestellt werden, folgt der nächste Schritt:"
                            )
                          )
                        ),
                        tags$li(
                          h3("Datenbearbeitung, Datenaufteilung und Überprüfung"),
                          tags$ul(
                            tags$li(
                              style = "font-size: 18px;",
                              "Es erfolgt eine Dummycodierung, um kategorische 
                              Variablen in numerische Werte zu transformieren, 
                              die von einigen Modellen besser verarbeitet werden 
                              können."
                            ),
                            tags$li(
                              style = "font-size: 18px;",
                              "Der Datensatz wird in Trainingsdaten (80 %) und 
                              Testdaten (20 %) aufgeteilt, um die Modelle zu 
                              evaluieren."
                            ),
                            tags$li(
                              style = "font-size: 18px;",
                              "Die Daten werden skaliert, sodass ihre Werte in 
                              einem Intervall zwischen 0 und 1 liegen."
                            )
                          )
                        ),
                        tags$li(
                          h3("Klassifikationsmodelle"),
                          tags$ul(
                            tags$li(
                              style = "font-size: 18px;",
                              "Es werden drei leistungsstarke Klassifikationsmodelle 
                              eingesetzt, um Kreditanträge zu bewerten."
                            ),
                            tags$li(
                              style = "font-size: 18px;",
                              "Logistische Regression als Standardvergleichsmodell: 
                              Dieses Modell dient als Benchmark für die komplexeren 
                              Modelle. Trotz seiner Einfachheit kann es effektiv 
                              sein und bietet den Vorteil der leichteren 
                              Interpretierbarkeit im Vergleich zu den anderen, 
                              komplexeren Modellen. Es wird hier eingesetzt, 
                              um eine Basislinie für die Leistung festzulegen 
                              und zu sehen, wie viel zusätzlichen Nutzen die 
                              komplexeren Modelle bieten können."
                            ),
                            tags$li(
                              style = "font-size: 18px;",
                              "xgBoost mit 50 Bäumen:",
                              "Hier wird die Ensemble-Technik verwendet, 
                              die 50 Entscheidungsbäume kombiniert, um fundierte 
                              Kreditentscheidungen zu treffen."
                            ),
                            tags$li(
                              style = "font-size: 18px;",
                              "15 Neuronale Netze mit gemittelten Prognosen:",
                              "Diese neuronalen Netze liefern jeweils eigene 
                              Vorhersagen, welche anschließend gemittelt werden, 
                              um eine robuste finale Prognose zu erhalten. Sie 
                              soll außerdem der reproduzierbarkeit dienen, da sich 
                              die Seedoption als schwierig erwies."
                            )
                          )
                        ),
                        tags$li(
                          h3("Untersuchung der Shapley Values"),
                          tags$ul(
                            tags$li(
                              style = "font-size: 18px;",
                              "Es folgt die Berechnung und Analyse der Shapley 
                              Values aller Modelle, um die Auswirkung einzelner 
                              Merkmale auf die Kreditprognosen zu verstehen."
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                # TP Datensatz ####
                tabItem(
                  tabName = "shap_datensatz",
                  fluidRow(
                    column(1),
                    column(
                      10,
                      h1("Überblick Datensatz"),
                      uiOutput("kab.feat"),
                      h1("Verteilung der Daten"),
                      tags$img(src = "https://baartz.solutions/Rplot02.svg", width = "1200px")
                    )
                  ),
                  fluidRow(
                    column(1),
                    column(
                      10,
                      h1(tags$u("Zusammenfassung")),
                      tags$div(
                        "Nach sorgfältiger Analyse des Datensatzes wurde festgestellt,
                          dass er keine fehlenden Werte aufweist. Besonders auffällig
                          ist die signifikante Korrelation zwischen dem Kreditscore
                          und dem Kreditstatus. Dies deutet darauf hin, dass der
                          Kreditscore wahrscheinlich den entscheidenden Einfluss
                          auf den Kreditantrag ausübt. Darüber hinaus gibt es im
                          Datensatz weitere bemerkenswerte Korrelationen. So besteht
                          beispielsweise eine starke Beziehung zwischen Luxusgütern
                          und Einkommen sowie zwischen der Kredithöhe und sowohl
                          Luxusgütern als auch Bankvermögen.
                          Überraschenderweise könnte die Kredithöhe einen geringeren
                          Einfluss auf den Kreditentscheid haben, als man zunächst
                          annehmen könnte. Ein weiterer interessanter Punkt für
                          die zukünftige Analyse, insbesondere bei der Auswertung
                          der Shapley Values, sind die negativen Werte der
                          Wohnimmobilien. Es könnte sein, dass Personen mit
                          negativen Wohnimmobilienwerten seltener einen Kredit
                          genehmigt bekommen. Zudem scheint es, dass kurze
                          Kreditlaufzeiten die Chancen auf eine Kreditgenehmigung
                          erhöhen könnten.",
                        br(),
                        br(),
                        "Im nächsten Schritt werden zunächst die Daten in Trainings-
                          und Testdaten aufgeteilt bevor auf die  Analyse der
                          Modellierung eingegangen wird. Dabei werden drei verschiedene
                          Ansätze in Betracht gezogen: die logistische Regression,
                          XGBoost und neuronale Netze.
                          Nachdem die Modelle erstellt wurden, wird ihre Performance
                          gründlich analysiert, um zu bestimmen, welches Modell
                          die besten Vorhersagen für den spezifischen Datensatz
                          liefert.
                          Nach der Analyse folgt die Interpretation der Modelle.
                          Ein besonderer Schwerpunkt wird dabei auf der Auswertung
                          der Shapley Values liegen. Diese bieten einen tiefen
                          Einblick in die Beiträge einzelner Merkmale zur Vorhersage
                          und können dabei helfen, die wichtigsten Treiber in den
                          Daten zu identifizieren.",
                        style = "font-size: 18px;"
                      )
                    )
                  )
                ),
                # TP Train Test ####
                tabItem(
                  tabName = "shap_train_test",
                  column(2),
                  column(
                    8,
                    h1("Aufteilung des Datensatzes"),
                    br(),
                    tags$div(
                      "Für das neuronale Netz wurden die Daten so  verwendet wie
                      sie hier zu sehen sind. Für das xgBoost Model wurde aus dem
                      Trainingsdatensatz noch ein Validierungsdatensatz mir rund
                      100 Daten extrahiert, um das Lernverhalten zu dokumentieren.",
                      style = "font-size: 18px;"
                    ),
                    box(
                      title = h1("Trainingsdaten"),
                      width = 6,
                      valueBoxOutput("train.data.prozent",width = 12),
                      valueBoxOutput("train.data.1",width = 12),
                      valueBoxOutput("train.data.0",width = 12),
                      valueBoxOutput("train.data.v1",width = 12),
                      valueBoxOutput("train.data.v2",width = 12),
                      valueBoxOutput("train.data.v3",width = 12),
                      valueBoxOutput("train.data.v4",width = 12),
                      valueBoxOutput("train.data.v5",width = 12),
                      valueBoxOutput("train.data.v6",width = 12),
                      valueBoxOutput("train.data.v7",width = 12),
                      valueBoxOutput("train.data.v8",width = 12),
                      valueBoxOutput("train.data.v9",width = 12),
                      valueBoxOutput("train.data.v10",width = 12),
                      valueBoxOutput("train.data.v11",width = 12)
                    ),
                    box(
                      title = h1("Testdaten"),
                      width = 6,
                      valueBoxOutput("test.data.prozent",width = 12),
                      valueBoxOutput("test.data.1",width = 12),
                      valueBoxOutput("test.data.0",width = 12),
                      valueBoxOutput("test.data.v1",width = 12),
                      valueBoxOutput("test.data.v2",width = 12),
                      valueBoxOutput("test.data.v3",width = 12),
                      valueBoxOutput("test.data.v4",width = 12),
                      valueBoxOutput("test.data.v5",width = 12),
                      valueBoxOutput("test.data.v6",width = 12),
                      valueBoxOutput("test.data.v7",width = 12),
                      valueBoxOutput("test.data.v8",width = 12),
                      valueBoxOutput("test.data.v9",width = 12),
                      valueBoxOutput("test.data.v10",width = 12),
                      valueBoxOutput("test.data.v11",width = 12)
                    ),
                    br(),
                    br(),
                    h1("Zusammenfassung"),
                    tags$div(
                      'Die Gesamtmenge der Daten wurde in Trainingsdaten (80%)
                      und Testdaten (20%) aufgeteilt. In den Trainingsdaten sind
                      insgesamt 3.415 Datensätze vorhanden, wobei 62.34% der
                      Kreditanträge genehmigt wurden (2.129 Datensätze) und 37.66 %
                      abgelehnt wurden (1.286 Datensätze). Die Verteilung der
                      Kreditgenehmigungen und -ablehnungen scheint somit
                      ausgeglichen zu sein, da kein Merkmal in einer der beiden
                      Kategorien überrepräsentiert ist.',
                      br(),
                      br(),
                      'Für die einzelnen Merkmale zeigen die Kennzahlen eine
                      ähnliche Verteilung zwischen Trainings- und Testdaten.
                      Die Anzahl der Familienangehörigen ("no_of_dependents")
                      variiert zwischen 0 und 5 in beiden Datensätzen. Das
                      Median-Jahreseinkommen ("income_annum") beträgt 5.100.000
                      in den Trainingsdaten und 5.100.000 in den Testdaten. Der
                      Kreditbetrag ("loan_amount") variiert zwischen 14.500.000
                      und 14.600.000 mit einem Medianwert von 14.600.000 in den
                      Trainingsdaten und 14.600.000 in den Testdaten.
                      Die Kreditlaufzeit ("loan_term") liegt zwischen 2 und
                      20 Jahren in beiden Datensätzen.',
                      br(),
                      br(),
                      'Der Kredit-Score ("cibil_score") variiert zwischen 300
                      und 900 in den Trainingsdaten und zwischen 300 und 603.5
                      in den Testdaten, wobei der Medianwert in beiden Fällen
                      nahe beieinander liegt (599 in Trainingsdaten und
                      603.5 in Testdaten).',
                      br(),
                      br(),
                      'Die Werte der Wohnimmobilien ("residential_assets_value")
                      liegen zwischen 5.600.000 und 5.700.000 mit einem Medianwert
                      von 5.700.000 in den Trainingsdaten und 5.700.000 in den
                      Testdaten. Die Werte der Gewerbeimmobilien
                      ("commercial_assets_value") variieren zwischen
                      3.700.000 und 3.500.000 mit einem Medianwert von 3.700.000
                      in den Trainingsdaten und 3.500.000 in den Testdaten.',
                      br(),
                      br(),
                      'Die Werte von Luxusgütern ("luxury_assets_value") variieren
                      zwischen 14.600.000 und 14.550.000 mit einem Medianwert von
                      14.600.000 in den Trainingsdaten und 14.550.000 in den Testdaten.
                      Der Wert der Bankvermögenswerte ("bank_asset_value") liegt
                      zwischen 4.600.000 und 4.500.000 mit einem Medianwert von
                      4.600.000 in den Trainingsdaten und 4.500.000 in den Testdaten.'
                      ,
                      br(),
                      br(),
                      'Die Anzahl der Personen mit Hochschulabschluss
                      ("education_Graduate") ist in den Trainingsdaten (1.693 Ja
                      und 1.722 Nein) und den Testdaten (451 Ja und 403 Nein)
                      ähnlich verteilt. Ebenso gibt es in den Trainingsdaten
                      (1.712 Ja und 1.703 Nein) und den Testdaten (407 Ja und 447
                      Nein) eine ähnliche Anzahl von Personen, die nicht
                      selbstständig sind ("self_employed_No").',
                      br(),
                      br(),
                      'Insgesamt scheint die Verteilung der Daten auf Test- und
                      Trainingsdaten in Ordnung zu sein, und es gibt kein Merkmal,
                      das in einer der beiden Kategorien überrepräsentiert ist.
                      Somit ist eine gute Voraussetzung für das Training eines
                      zuverlässigen Modells und das Testen seiner Leistungsfähigkeit
                      geschaffen worden.',
                      style = "font-size: 18px;"
                    )
                    
                    
                  )
                ),
                # Modelle ####
                ## Information ####
                tabItem(
                  tabName = "fit_info",
                  column(2),
                  column(
                    8,
                    h1("Vorgehen"),
                    tags$div(
                      "Zu Beginn des Modellierungsprozesses wurden die Daten
                      in Trainings- und Testsets aufgeteilt. Um die Daten für
                      die Modellierung vorzubereiten, wurden die Trainingsdaten
                      skaliert, sodass ihre Werte in einem Intervall zwischen 0
                      und 1 liegen. Dies wurde mit der folgenden Formel erreicht:
                      $$y=\\frac{x-min(x)}{max(x)-min(x)}$$
                      Es ist von entscheidender Bedeutung, dass die Testdaten
                      mit denselben Skalierungswerten wie die Trainingsdaten
                      skaliert werden, um sicherzustellen, dass die Modelle
                      später korrekte Vorhersagen für unbekannte Daten liefern
                      können. Nachdem die Skalierung abgeschlossen war, begann
                      die Modellierung.
                      ",
                      
                      br(),
                      br(),
                      
                      "Während des Modellierungsprozesses war es von zentraler
                      Bedeutung, sicherzustellen, dass alle Modelle unter denselben
                      Bedingungen erstellt wurden, um die Ergebnisse vergleichbar zu
                      machen. Bei der logistischen Regression ist die Reproduzierbarkeit
                      durch das zugrunde liegende mathematische Modell gegeben.
                      Im Gegensatz dazu verwendet XGBoost Zufallszahlen, was zu
                      unterschiedlichen Ergebnissen bei wiederholten Durchläufen
                      führen kann. Dieses Problem kann durch das Setzen eines Seeds
                      gelöst werden. Bei neuronalen Netzen, insbesondere bei solchen,
                      die mit Keras/Tensorflow erstellt wurden, ist das Setzen eines
                      Seeds jedoch komplizierter. Um trotzdem eine gewisse
                      Reproduzierbarkeit zu gewährleisten, wurden 15 verschiedene
                      neuronale Netze erstellt. Der Gedanke dahinter ist, dass
                      der Mittelwert der Vorhersagen und später der Mittelwert der
                      Shapley Values über diese Modelle hinweg konsistente und
                      ähnliche Ergebnisse liefern sollte.
                      ",
                      style = "font-size: 18px;"
                    )
                  )
                ),
                ## Logistic Regression ####
                tabItem(
                  tabName = "fit_lr",
                  fluidRow(
                    column(1),
                    column(
                      8,
                      h1("Logistische Regression"),
                      div(
                        class = "customSummary",
                        verbatimTextOutput("tmp.lr")
                      )
                    )
                  ),
                  fluidRow(
                    column(1),
                    column(
                      8,
                      plotlyOutput("lr.conf"),
                    ),
                    column(
                      3,
                      h3(tags$u("Erklärung")),
                      tags$div(
                        'Die Confusionsmatrix zeigt die Leistung des
                        Kreditantragsklassifikators anhand von Vorhersagen zur
                        Genehmigung oder Ablehnung von Kreditanträgen.
                        Die Matrix ist in vier Teile unterteilt:',
                        br(),
                        br(),
                        'True Positive (TP): Die Anzahl der Kreditanträge,
                        die korrekt als genehmigt vorhergesagt wurden.
                        Das bedeutet, dass das Modell diese Kreditanträge
                        richtig erkannt hat und sie tatsächlich genehmigt wurden.',
                        br(),
                        br(),
                        'True Negative (TN): Die Anzahl der Kreditanträge,
                        die korrekt als abgelehnt vorhergesagt wurden. Das Modell
                        hat diese Anträge richtig erkannt und sie wurden tatsächlich
                        abgelehnt.',
                        br(),
                        br(),
                        'False Positive (FP): Die Anzahl der Kreditanträge, die
                        fälschlicherweise als genehmigt vorhergesagt wurden.
                        Diese Anträge wurden fälschlicherweise für gut befunden,
                        obwohl sie abgelehnt wurden. ',
                        br(),
                        br(),
                        'False Negative (FN): Die Anzahl der Kreditanträge, die
                        fälschlicherweise als abgelehnt vorhergesagt wurden. Das
                        Modell hat diese Anträge fälschlicherweise für ungeeignet
                        befunden, obwohl sie genehmigt wurden. Diese werden auch
                        als verpasste Chance bezeichnet.',
                        br(),
                        br(),
                        'Basierend auf diesen Werten können verschiedene
                        Bewertungsmetriken berechnet werden, um die Leistung des
                        Modells zu bewerten. Die Genauigkeit (Accuracy) misst den
                        Prozentsatz der korrekten Vorhersagen insgesamt und wird
                        als Überschrift in dem Cofusionsplot dargestellt. '
                      )
                    )
                  ),
                  fluidRow(
                    visNetworkOutput(
                      "network.lr",
                      width = "100%",
                      height="800px"
                    )
                  ),
                  column(1),
                  column(
                    8,
                    br(),
                    br(),
                    h1("Zusammenfassung"),
                    tags$div(
                      "Die Genauigkeit (Accuracy) der logistischen Regression
                      liegt bei knapp 92 % sowohl für die Trainings- als auch für
                      die Testdaten. Bei der Betrachtung des Modells fällt auf,
                      dass lediglich vier Merkmale einen signifikanten Einfluss
                      haben. Üblicherweise folgt nun eine Phase der Modellstraffung, 
                      in der weniger relevante Merkmale nach und nach ausgeschlossen 
                      werden. Für den späteren Vergleich der Shapley Values 
                      zwischen den Modellen
                      ist es jedoch notwendig, alle Merkmale beizubehalten. Ein
                      weiterer Punkt, der Beachtung finden könnte, ist die
                      Multikollinearität, also die Korrelation der Merkmale
                      untereinander. Dies könnte intensiver untersucht werden.
                      Zudem könnten Daten-Transformationen, wie beispielsweise
                      das Logarithmieren, zu einer erhöhten Signifikanz einiger
                      Merkmale führen. Diese potenziellen Herangehensweisen werden
                      in der weiteren Modellierung jedoch nicht berücksichtigt,
                      da sie nicht im Fokus dieser Arbeit stehen.",
                      style = "font-size: 18px;"
                    )
                  )
                ),
                tabItem(
                  ## Neuronale Netze ####
                  tabName = "fit_nn",
                  fluidRow(
                    column(1),
                    column(
                      8,
                      h1("Neuronales Netz"),
                      br(),
                      br(),
                      tags$div(
                        "Die neuronalen Netze wurde mittels Keras erzeugt.
                        Ursprünglich war Keras eine eigenständige Bibliothek,
                        die verschiedene Deep Learning Backends unterstützte,
                        einschließlich TensorFlow., Theano und Microsoft Cognitive
                        Toolkit (CNTK). Im Laufe der Zeit wurde TensorFlow das
                        dominierende Backend für Keras, und schließlich wurde
                        Keras offiziell in TensorFlow integriert.",
                        br(),
                        br(),
                        "Die neuronalen Netze wurde mit drei versteckten Schichten
                        konzipiert. Die Eingangsschicht, sowie die drei versteckten
                        Schichten nutzen die ReLU-Aktivierungsfunktion.
                        ReLU steht für Rectified Linear Unit Funktion
                        und ist wie folgt definiert:
                        $$f(x) = max(0,x)$$
                        ReLU hat den Vorteil, dass sie nicht sättigt, wenn x>0 ist.
                        Das bedeutet, ass sie während des Trainings weniger anfällig
                        für das Problem des verschwindenden Gradienten ist, was das
                        Training beschleunigen kann. Zudem ist die Funktion
                        und ihre Ableitung einfach zu berechnen.",
                        br(),
                        br(),
                        "Die Ausgangsschicht nutzt die Sigmoid-Funktion:
                        $$\\sigma(x) = \\frac{1}{1+e^{-x}}$$
                        Die Sigmoid-Funktion gibt Werte zwischen 0 und 1 zurück,
                        was besonders nützlich ist, wenn das Netzwerk Wahrscheinlichkeiten
                        vorhersagen soll, wie es bei binären Klassifikationsproblemen
                        der Fall ist. Außerdem ist sie überall differenzierbar, was für das
                        Gradientenabstiegsverfahren essentiell ist.",
                        br(),
                        br(),
                        "Der Lernparameter für  den Adam-Optimierer,
                        der für die Optimierung des Gradientenabstiegs
                        zuständig ist, wurde auf den Wert 0.001 festgelegt.
                        Der Trainingsdatensatz wurde 75 Mal durch das Netzwerk geführt,
                        um die Gewichte der Neuronen zu optimieren. Beim stochastischen
                        Gradientenabstieg werden die Gewichte nicht nach jeder Datenprobe,
                        sondern nach einer Gruppe von Proben, den sogenannten 'Batches',
                        aktualisiert. Dieser Wert wurde für die Modellierung
                        auf 32 festgelegt. Das gesamte Verfahren wurde 15 Mal
                        wiederholt, um die Mittelwerte der Prognosen und
                        Shapley Values zu reproduzieren, da sich die Verwendung eines
                        Seeds als kompliziert herausstellte.",
                        br(),
                        br(),
                        style = "font-size: 18px;"
                      ),
                      sliderInput(
                        'nn.fits',
                        'Wähle fit',
                        min = 1,
                        max = 15,
                        step = 1,
                        value = 1,
                        animate = T,
                        width = "100%"
                        )
                    ),
                    column(3)
                  ),
                  fluidRow(
                    column(1),
                    column(
                      8,
                      plotlyOutput("nn.fits.metrics"),
                      sliderInput(
                        'nn.fits2',
                        'Wähle fit',
                        min = 1,
                        max = 15,
                        step = 1,
                        value = 1,
                        animate = T,
                        width = "100%"),
                      plotlyOutput("nn.fits.metrics2")
                    ),
                    column(
                      3,
                      h3(tags$u("Erklärung")),
                      h5('Verlustfunktion ("loss")'),
                      tags$div(
                        'Die Grafik zeigt den Verlauf
                        der Verlustfunktion ("loss") und der Validierungsverlustfunktion
                        ("val_loss") über 75 Epochen der trainierten neuronalen
                        Netze. Mit dem Slider lassen sich alle 15 verwendeten Modelle
                        auswählen und analysieren. Die y-Achse repräsentiert
                        die Verlustwerte, während die x-Achse die Anzahl der
                        Epochen darstellt. Die grüne Linie zeigt den
                        Trainingsverlust, die orangefarbene Linie den
                        Validierungsverlust. Ein abnehmender Verlauf der grünen
                        Linie zeigt, dass das Modell gut lernt, während eine
                        Abweichung zwischen den Linien auf Overfitting hindeuten kann.',
                        br(),
                        br(),
                        
                        h5('Genauigkeitsfunktion ("accuracy")'),
                        'Die zweite Grafik zeigt den Verlauf der Genauigkeitsfunktion
                        ("accuracy") und der Validierungsgenauigkeitsfunktion
                        ("val_accuracy") über 75 Epochen der trainierten neuronalen
                        Netze.',
                        br(),
                        br(),
                        'Die Genauigkeitsfunktion misst, wie gut das Modell die
                        richtigen Vorhersagen für die Trainingsdaten trifft.
                        Sie gibt den Prozentsatz der korrekten Vorhersagen im
                        Vergleich zu den tatsächlichen Werten an.
                        ',
                        br(),
                        br(),
                        'Während des Trainings strebt das Modell danach, die
                        Trainingsgenauigkeit zu maximieren, indem es seine
                        Modellparameter anpasst und sich an die Trainingsdaten
                        anpasst. Ein ansteigender Verlauf der Linien deutet
                        darauf hin, dass das Modell besser wird und korrektere
                        Vorhersagen für die Trainings- und Validierungsdaten trifft.'
                      )
                    )
                  ),
                  fluidRow(
                    column(1),
                    column(
                      8,
                      plotlyOutput("nn.conf"),
                      br(),
                      sliderInput(
                        'nn.fits3', 
                        'Wähle fit', 
                        min = 1,
                        max = 15,
                        step = 1,
                        value = 1,
                        width = "100%"
                        ),
                      sliderInput(
                        'nn.fits4', 
                        'Wähle Layer', 
                        min = 1,
                        max = 3,
                        step = 1,
                        value = 1,
                        width = "100%"
                        )
                    ),
                    column(
                      3,
                      h3(tags$u("Erklärung")),
                      tags$div(
                        'Die Confusionsmatrix zeigt die Leistung des
                        Kreditantragsklassifikators anhand von Vorhersagen zur
                        Genehmigung oder Ablehnung von Kreditanträgen.
                        Die Matrix ist in vier Teile unterteilt:',
                        br(),
                        br(),
                        'True Positive (TP): Die Anzahl der Kreditanträge,
                        die korrekt als genehmigt vorhergesagt wurden.
                        Das bedeutet, dass das Modell diese Kreditanträge
                        richtig erkannt hat und sie tatsächlich genehmigt wurden.',
                        br(),
                        br(),
                        'True Negative (TN): Die Anzahl der Kreditanträge,
                        die korrekt als abgelehnt vorhergesagt wurden. Das Modell
                        hat diese Anträge richtig erkannt und sie wurden tatsächlich
                        abgelehnt.',
                        br(),
                        br(),
                        'False Positive (FP): Die Anzahl der Kreditanträge, die
                        fälschlicherweise als genehmigt vorhergesagt wurden.
                        Diese Anträge wurden fälschlicherweise für gut befunden,
                        obwohl sie abgelehnt wurden. ',
                        br(),
                        br(),
                        'False Negative (FN): Die Anzahl der Kreditanträge, die
                        fälschlicherweise als abgelehnt vorhergesagt wurden. Das
                        Modell hat diese Anträge fälschlicherweise für ungeeignet
                        befunden, obwohl sie genehmigt wurden. Diese werden auch
                        als verpasste Chance bezeichnet.',
                        br(),
                        br(),
                        'Basierend auf diesen Werten können verschiedene
                        Bewertungsmetriken berechnet werden, um die Leistung des
                        Modells zu bewerten. Die Genauigkeit (Accuracy) misst den
                        Prozentsatz der korrekten Vorhersagen insgesamt und wird
                        als Überschrift in dem Cofusionsplot dargestellt. '
                      )
                    )
                  ),
                  fluidRow(
                    column(1),
                    column(
                      8,
                      plotlyOutput("nn.heat")
                    ),
                    column(
                      3,
                      h3(tags$u("Erklärung")),
                      tags$div(
                        "Das neuronale Netz besteht aus drei versteckten Schichten,
                        von denen jede 16 Neuronen enthält. Mit den beiden Slidern
                        kann eines der 15 trainierten neuronalen Netze und
                        eine der drei versteckten Schichten ausgewählt werden.
                        Die Heatmap zeigt die Gewichte der ausgewählten Schicht
                        des ausgewählten Netzes. Die Gewichte
                        der Neuronen lassen keine klare Interpretation zu."
                      )
                    )
                  ),
                  fluidRow(
                    column(1),
                    column(
                      8,
                      column(
                        6, 
                        h2("Kanten mit Gewichten"),
                        DTOutput("nndata")
                      ),
                      column(
                        6,
                        h2("Knoten - mit Bias"),
                        DTOutput("nndata2")
                      ),
                      sliderInput(
                        'nn.fits5',
                        'Wähle fit',
                        min = 1,
                        max = 15,
                        step = 1,
                        value = 1,
                        width = "100%")
                    ),
                    column(
                      3,
                      br(),
                      br(),
                      h3("Erklärung"),
                      tags$div(
                        "Die linke Tabelle zeigt die Verbindungen zwischen den
                        Knoten (From-To) im neuronalen Netz sowie die
                        dazugehörigen Gewichte. In der zweiten Tabelle sind die
                        Bias-Werte für die insgesamt 49 Neuronen aufgeführt. Die
                        Eingangsschicht (11 Neuronen) besitzen keinen Bias.
                        Diese Daten wurden verwendet, um das folgende
                        neuronale Netzwerk darzustellen. Mit dem Slider kann
                        zwischen allen 15 Netzen ausgewählt werden, die alle
                        die gleiche Struktur haben, jedoch unterschiedliche
                        Gewichte und Bias aufweisen.
                        Die Eingangsschicht ist durch eine eckige orangefarbene
                        Box dargestellt, während die versteckten Schichten in lila
                        gehalten sind. Das Ausgangslayer, bzw. das Zielneuron,
                        wird als orangefarbener Stern dargestellt."
                      )
                    )
                  ),
                  fluidRow(
                    visNetworkOutput(
                      "network",
                      width = "100%",
                      height="800px"
                    )
                  ),
                  fluidRow(
                    column(2),
                    column(
                      8,
                      br(),
                      br(),
                      h1("Zusammenfassung"),
                      tags$div(
                        "Die Verlust- und Genauigkeitsmetriken über die 15
                        erstellten Netze deuten auf eine solide Modellbasis hin.
                        Bei den Trainingsdaten zeigt die Konfusionsmatrix eine
                        beeindruckende Genauigkeit von 98 %. Bei den Testdaten
                        wurde eine Genauigkeit von 96 % erreicht. Obwohl aus der
                        Heatmap der Neuronengewichte der einzelnen Schichten keine
                        direkten Schlussfolgerungen abgeleitet werden konnten,
                        bietet die grafische Darstellung des neuronalen Netzes
                        einen klaren Einblick in dessen Komplexität. Sie
                        verdeutlicht, dass eine direkte Interpretation der
                        Gradienten bzw. Neuronenaktivierungen nicht praktikabel
                        ist. Daher werden die Shapley-Values herangezogen, um
                        die Modellergebnisse zu interpretieren.",
                        br(),
                        br(),
                        "Zur weiteren Optimierung der Ergebnisse könnte die
                        Dropout-Technik angewendet werden, um Overfitting zu
                        verhindern. Dabei wird während des Trainings zufällig ein
                        bestimmter Prozentsatz der Neuronen 'ausgeschaltet', was
                        als Regularisierungsmethode dient. Zusätzlich könnten
                        Experimente mit unterschiedlichen Lernraten durchgeführt
                        oder alternative Optimierer getestet werden.
                        Nichtsdestotrotz sind die aktuellen Lern- und
                        Genauigkeitsmetriken für die anschließende Modellierung
                        der Shapley-Values ausreichend.",
                        style = "font-size: 18px;"
                      )
                    )
                  )
                ),
                tabItem(
                  ## xgboost ####
                  tabName = "fit_xg",
                  fluidRow(
                    column(1),
                    column(
                      8,
                      h1("XGBoost"),
                      tags$div(
                        "XGBoost, kurz für 'Extreme Gradient Boosting', ist eine
                        optimierte Implementierung des Gradient Boosting-Algorithmus.
                        Es wurde speziell entwickelt, um sowohl in Bezug auf die
                        Modellleistung als auch auf die Rechenzeit effizient
                        zu sein. XGBoost hat sich in vielen maschinellen
                        Lernwettbewerben und -projekten als leistungsstark
                        erwiesen und bietet eine Vielzahl von Funktionen und
                        Optimierungen, die es zu einem bevorzugten Werkzeug für
                        viele Datenwissenschaftler machen.",
                        br(),
                        br(),
                        "Für die Modellierung in diesem Projekt wurde XGBoost mit
                        dem Ziel der binären Klassifikation (binary:logistic)
                        verwendet. Das Modell verwendet den Gradient Boosting-Algorithmus
                        (booster = 'gbtree') und eine Reihe von Hyperparametern,
                        um den Lernprozess zu steuern. Die Lernrate (eta) wurde auf
                        0,01 festgelegt, er bestimmt, wie schnell das Modell
                        auf die Daten reagiert. Die maximale Tiefe der Bäume
                        (max_depth) wurde auf 6 festgelegt, um die Komplexität des
                        Modells zu steuern. Weitere Parameter wie gamma,
                        min_child_weight, subsample und colsample_bytree wurden
                        ebenfalls festgelegt, um den Trainingsprozess zu optimieren
                        und Overfitting zu verhindern.",
                        br(),
                        br(),
                        "Während des Trainingsprozesses wurde eine Watchlist
                        verwendet, um die Leistung sowohl des Trainings- als auch
                        des Validierungsdatensatzes zu überwachen. Das Modell
                        wurde für 50 Runden (nrounds) trainiert, wobei in jeder
                        Runde die Leistung des Modells überwacht und angepasst
                        wurde, um die bestmögliche Vorhersagegenauigkeit zu erzielen.",
                        style = "font-size: 18px;"
                      )
                    )
                  ),
                  fluidRow(
                    column(1),
                    column(
                      8,
                      br(),
                      br(),
                      plotlyOutput("xg.fits.metrics")
                    ),
                    column(
                      3,
                      h3(tags$u("Erklärung")),
                      div(
                        "In diesem Plot werden die Fehlerwerte eines xgboost-Modells
                        während des Trainings für ein Kreditklassifikationsproblem
                        dargestellt. Die grüne Linie zeigt den Trainingsfehler,
                        der angibt, wie gut das Modell die Kreditanträge in den
                        Trainingsdaten vorhersagt. Das Modell versucht den
                        Trainingsfehler während des Trainingsprozesses zu minimieren,
                        um sich gut an die Trainingsdaten anzupassen.",
                        br(),
                        br(),
                        "Die orangefarbene Linie zeigt den Validierungsfehler,
                        der den Fehler auf neuen, nicht-gesehenen Daten angibt,
                        die als Validierungsdaten verwendet werden. Eine gute
                        Generalisierung wird erreicht, wenn der Validierungsfehler
                        niedrig bleibt und das Modell auf unbekannte
                        Kreditanträge gut vorhersagt.",
                        br(),
                        br(),
                        "Das Diagramm ermöglicht es, den Trainingsfortschritt zu
                        überwachen und sicherzustellen, dass das Modell sowohl
                        eine gute Anpassung an die Trainingsdaten als auch eine
                        gute Generalisierungsfähigkeit für neue Kreditanträge
                        aufweist. Eine optimale Leistung des Modells kann durch
                        die Analyse dieser Fehlerkurven erreicht werden."
                      )
                    )
                    
                  ),
                  fluidRow(
                    column(1),
                    column(
                      8,
                      plotlyOutput("xg.conf"),
                      br(),
                      selectInput(
                        "xg.train.test2",
                        "Trainings- oder Testdaten",
                        choices = c("Trainingsdaten"=T,"Testdaten"=F)
                        )
                    ),
                    column(
                      3,
                      h3(tags$u("Erklärung")),
                      tags$div(
                        "Die Erklärung zur Konfusionsmatrix wurde bereits bei den
                        neuronalen Netzen näher erläutert."
                      )
                    )
                  ),
                  fluidRow(
                    column(1),
                    column(
                      8,
                      sliderInput(
                        'xg.fits3', 
                        'Wähle einen Baum', 
                        min = 1,
                        max = 50,
                        step = 1,
                        value = 1,
                        width = "100%"
                        ),
                      visNetworkOutput(
                        "network.xg", 
                        width = "100%", 
                        height="1000px"
                        )
                    ),
                    column(
                      3,
                      h3(tags$u("Erklärung")),
                      tags$div(
                        "Dieses dynamische Baum-Diagramm zeigt die
                        Entscheidungsregeln des XGBoost-Modells, das für die
                        Genehmigung oder Ablehnung von Kreditanträgen verwendet
                        wird. Es besteht aus insgesamt 50 Bäumen.
                        Jeder Baum trifft eine Reihe von Entscheidungen, um zu
                        bestimmen, ob ein Kreditantrag genehmigt oder abgelehnt
                        wird. Die Entscheidungen basieren auf verschiedenen
                        Informationen, die über den Kreditantrag vorliegen, wie
                        zum Beispiel das Einkommen des Antragstellers und andere
                        relevante Merkmale. Es kann jeder
                         einzelne Baum betrachtet werden, um ein Gefühl dafür
                         zu bekommen, wie sich die Entscheidungsregeln zusammensetzen.
                         Das Modell kombiniert die Entscheidungen aus den 50 Bäumen,
                        um eine endgültige Vorhersage zu treffen, ob der Kreditantrag
                        angenommen oder abgelehnt wird. ",
                        br(),
                        br(),
                        'Die Zahlen auf den Kanten stellen Fragen dar, ob die
                        Bedingungen "kleiner" erfüllt ist. Wenn die Antwort "ja" ist,
                        muss dem Pfad gefolgt werden, ansonsten nimmt es den anderen Pfad.
                        Die finale Prognose in einem XGBoost-Modell setzt sich aus der
                        Summe der Vorhersagen aller Bäume zusammen.
                        ',
                        h5("Gain"),
                        'Gain oder auch "Gewinn" ist eine Metrik, die die
                        Bedeutung eines Features bei der Klassifizierung von
                        Datenpunkten in einem Entscheidungsbaum bewertet. Er
                        misst den beigetragenen Verbesserungswert (Gewinn) für
                        die Reduzierung des Verlustes, der durch die Teilung der
                        Daten durch das betrachtete Feature erzielt wird.
                        Mit anderen Worten, der Gain zeigt an, wie viel besser
                        der Entscheidungsbaum durch die Berücksichtigung eines
                        bestimmten Features geworden ist.',
                        br(),
                        h5("Cover"),
                        'Cover oder "Abdeckung" ist eine Metrik, die angibt, wie
                        viele Datenpunkte durch die Entscheidungsregel eines
                        Knotens im Entscheidungsbaum abgedeckt werden. Es misst
                        die Anzahl der Datenpunkte, die durch die Entscheidungsregel
                        dieses Knotens berücksichtigt werden. Eine hohe Cover-Metrik
                        zeigt an, dass der Knoten einen großen Teil der Daten im
                        Trainingsdatensatz abdeckt.',
                        br(),
                        br(),
                        'In XGBoost wird der "Gain" verwendet, um die Relevanz
                        von Features zu bewerten und den Entscheidungsbaum so zu
                        formen, dass es die relevantesten Features berücksichtigt.
                        Die "Cover"-Metrik dient dazu, die Effizienz des
                        Entscheidungsbaums zu bewerten und zu überprüfen, wie
                        viele Datenpunkte in den einzelnen Knoten berücksichtigt
                        werden.'
                      )
                    )
                  ),
                  column(2),
                  column(
                    8,
                    h1("Zusammenfassung"),
                    tags$div(
                      "Die erzielte Fehlerrate sowohl für die Trainings- als auch
                      für die Testdaten spricht für eine erfolgreiche Modellierung.
                      Die Genauigkeit basierend auf den Trainingsdaten beträgt
                      98,5 %, während sie für die Testdaten bei 97 % liegt.
                      Bei der Prognose einer neuen Instanz wird jeder der Bäume
                      durchlaufen, wobei die Werte der 50 Blätter am Ende summiert
                      werden, um die endgültige Prognose zu bilden.
                      Diese Herangehensweise unterstreicht die Komplexität des
                      Modells und macht deutlich, dass eine direkte Interpretation
                      der Ergebnisse nicht praktikabel ist. Aus diesem Grund
                      werden die Shapley-Values zur Interpretation der
                      Modellergebnisse verwendet.",
                      br(),
                      br(),
                      "Zur weiteren Optimierung des Modells könnten Experimente
                      mit verschiedenen Parametereinstellungen durchgeführt werden.
                      Angesichts der Tatsache, dass es insgesamt nur 11 Merkmale
                      gibt und die logistische Regression bereits aufgezeigt hat,
                      dass lediglich vier davon signifikant sind, könnte eine
                      Verringerung der Baumtiefe in Erwägung gezogen werden.
                      Dies könnte zudem dazu beitragen, Overfitting zu reduzieren.",
                      br(),
                      br(),
                      "Abschließend lässt sich sagen, dass die aktuellen Lern- und
                      Genauigkeitsmetriken für die nachfolgende Analyse der
                      Shapley-Values vollkommen ausreichend sind.",
                      style = "font-size: 18px;"
                    )
                  )
                ),
                # SHAPley Values  ####
                ## Info ####
                tabItem(
                  tabName = "shap_info",
                  fluidRow(
                    column(2),
                    column(
                      8,
                      h1("Shapley Values"),
                      tags$div(
                        "Die Shapley-Werte wurden erstmals im Jahr 1953 von Lloyd 
                        S. Shapley als Teil der Spieltheorie
                        für n-Personen-Spiele eingeführt. Sie bieten eine mathematisch 
                        fundierte Methode zur 'fairen' Aufteilung von Gewinnen in 
                        Koalitionsspielen. In jüngerer Zeit werden sie auch als 
                        Instrument zur Interpretation von maschinellem Lernen und 
                        KI-Modellen anerkannt, da sie eine systematische Methode 
                        bieten, um zu quantifizieren, wie viel jedes Eingabemerkmal 
                        zur Vorhersage eines Modells beiträgt.",
                        br(),
                        br(),
                        h3("Regression"),
                        "Bei linearen Modellen kann die Vorhersage durch die errechneten
                        Koeffizienten $\\beta$  als linear Kombination errechnet werden:
                        $$\\hat{f}(x)=\\beta_0+\\beta_1 x_1+...+\\beta_p x_p$$
                        Der Beitrag $\\phi$ für ein Merkmal $x_p$ errechnet sich dann:
                        $$\\phi_j(\\hat{f})=\\beta_j x_j -E(\\beta_jX_j)=\\beta_jx_j-\\beta_jE(X_j)$$
                        Wenn alle Beiträge $\\phi_j$ einer Instanz aufsummiert werden ergibt sich
                        die Prognose abzüglich des durchschnittlichen Prognosewertes:
                        \\begin{align}
                        \\sum_{j=1}^{p} \\phi_j(\\hat{f}) &= \\sum_{j=1}^{p} 
                        (\\beta_j x_j - E(\\beta_j X_j)) \\\\
                        &=(\\beta_0 + \\sum_{j=1}^{p} \\beta_j x_j) - 
                        (\\beta_0 + \\sum_{j=1}^{p} E(\\beta_j X_j)) \\\\
                        &=\\hat{f}(x) - E(\\hat{f}(X))
                        \\end{align}

                        Diese Berechnung kann in der Form für komplexere Modelle nicht
                        direkt angewandt werden, weshalb auf die Shapley Values aus der
                        Spieltheorie zurückgegriffen werden.
                        ",
                        h3("Komplexere Modelle"),
                        "Für komplexere Modelle  erfolgt die Berechnung nach der 
                        folgenden Methode:
                        \\begin{align}
                        \\phi_i(f,x)= \\sum_{z'\\subseteq x'} 
                        \\frac{|z'|!(M-|z'|-1)!}{M!}(f_x(z')-f_x(z')\\backslash i)
                        \\end{align}",
                        "Diese Gleichung erfüllt die Eigenschaften der lokalen Genauigkeit,
                        Missingness und Konsitenz auf die hier nicht weiter eingegangen wird.",
                        br(),
                        br(),
                        "Für die Berechnung der Shapley Values wird für das
                        neuronale Netz  Deeplift aus dem Package innsight verwendet. Für
                        xgBoost gibt es ein eigens entwickeltes Package namens SHAPforxgboost.
                        Alternativ kann auch die in R integrierte predict Funktion
                        mit der Option 'predcontrib = TRUE' verwendet werden. Für die
                        logistische Regression erfolgt die Berechnung nach der oben beschriebenen Methode.",
                        style = "font-size: 18px;"
                      ),
                      fluidRow(
                        column(2),
                        column(
                          4,
                          selectInput(
                            inputId = "subdata.train",
                            label = "Wähle Daten",
                            choices = c(
                              "Trainingsdaten"="A",
                              "Testdaten" = "B"
                              ),
                            selected = "A"
                          )
                        ),
                        column(
                          4,
                          selectInput(
                            "subdata.pred",
                            label = "Wähle Abzisse",
                            choices = c(
                              "Plot gegen Vorhersage"="A",
                              "Plot gegen 'richtigen' Kreditstatus" = "B"
                            )
                          )
                        )
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      4,
                      h3("XgBoost"),
                      plotlyOutput("shap.ziel.xg")
                    ),
                    column(
                      4,
                      h3("Neuronales Netz"),
                      plotlyOutput("shap.ziel.nn")
                    ),
                    column(
                      4,
                      h3("Logistische Regression"),
                      plotlyOutput("shap.ziel.lr")
                    )
                  )
                ),
                tabItem(
                  ## Logistic Regression ####
                  tabName = "shap_lr",
                  tags$head(
                    tags$style(
                      HTML(".centered { text-align: center; }")
                    )
                  ) %>%
                    h1("SHAP-Abhängigkeits- und Interaktionsdiagramme", class = "centered"),
                  fluidRow(
                    column(2),
                    column(
                      8,
                      tags$div(
                        h3("Featureimportance"),
                        "In der ersten Grafik sind die Merkmale nach Wichtigkeit
                        sortiert. Wie bereits bei der Modellierung der logistischen 
                        Regression festgestellt wurde, sind die vier Merkmale 
                        Kredit Score (cibil score),nEinkommen (income_annum), 
                        Kredithöhe (loan_amount) und Kreditlaufzeit
                        (loan_term) die wichtigsten Einflussfaktoren.
                        Die stark grünen Daten weisen auf eine nidrige Werte des 
                        Merkmals hin, wohingegen die gelben Datenpunkte auf hohe 
                        Werte hindeuten. Aus den Abbildung lässt sich ableiten, 
                        dass eine Person mit einem hohen Kreditscore und einem 
                        geringeren Einkommen, gepaart mit einem hohen Kreditbetrag 
                        und kurzer Laufzeit sehr wahrscheinlich einen Kredit erhält.",
                        style = "font-size: 18px;"
                      ),
                    )
                  ),
                  fluidRow(
                    column(
                      2,
                      "Daten:",
                      switchInput(
                        inputId = "shap.lr.slider",
                        size = "mini",
                        onStatus=T,
                        offStatus = F,
                        onLabel = "Train",
                        offLabel = "Test",
                        value=F
                      )
                    ),
                    column(
                      8,
                      plotlyOutput("shap.lr.ind")
                    )
                  ),
                  fluidRow(
                    column(2),
                    column(
                      8,
                      div(
                        h3("Abhängigkeits und Interaktionsdiagramm"),
                        "Die zweite Grafik ist für die logistische Regression 
                        nicht sinnvoll,denn die Shaple Werte eines Merkmals 
                        verlaufen Proportional zu der Ausprägung des Merkmals. 
                        Dieser Zusammenhang ist mathamtisch begründet.
                        Die Grafik ist dennoch wichtig, um den Zusammenhang 
                        zwischen der  logistischen Regression und den nicht 
                        linearen Modellen xgBoost und den neuronalen Netzen zu 
                        verstehen.",
                        style = "font-size: 18px;"
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      2,
                      selectInput(
                        inputId = "shap.lr.slid.txt1",
                        label = "Wähle Feature Abzisse",
                        choices = colnames(loan_data)[c(-1,-12)],
                        selected = "cibil_score"
                      ),
                      selectInput(
                        inputId = "shap.lr.slid.txt2",
                        label = "Wähle SHAP Ordinate",
                        choices = colnames(loan_data)[c(-1,-12)],
                        selected = "cibil_score"
                      )
                    ),
                    column(
                      10,
                      plotlyOutput("shap.lr.depend"),
                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      h1("Individuele Prognose"),
                      fluidRow(
                        column(
                          5,
                          uiOutput("txt.lr"),
                          br(),
                          sliderInput(
                            "user00.lr",
                            label = "Wähle individuellen User:",
                            min = 1,
                            max = 4269,
                            value = 3540,
                            step = 1,
                            width = "100%",
                            animate = T
                          ),
                          plotlyOutput("shap.lr.idividuell")
                        ),
                        column(
                          5,
                          DTOutput("User.data.lr"))
                      )
                    )
                  ),
                  fluidRow(
                    column(2),
                    column(
                      8,
                      h1("Heatmap Shapley Userdaten"),
                      sliderInput(
                        "range.lr",
                        "Bereich",
                        min=100, 
                        max=nrow(loan_data),
                        step=10,
                        value = 100,
                        width="100%",
                        animate = T
                        ),
                      plotlyOutput("heat.shap.lr")
                    )
                  )
                ),
                ## Neuronales Netz ####
                tabItem(
                  tabName = "shap_nn",
                  tags$head(
                    tags$style(
                      HTML(".centered { text-align: center; }")
                    )
                  ) %>%
                    h1("SHAP-Abhängigkeits- und Interaktionsdiagramme", class = "centered"),
                  fluidRow(
                    column(2),
                    column(
                      8,
                      tags$div(
                        h3("Featureimportance"),
                        "In der ersten Grafik sind die Merkmale nach Wichtigkeit 
                        sortiert. Wie bereits bei der Modellierung der logistischen 
                        Regression festgestellt wurde, sind die vier Merkmale 
                        Kredit Score (cibil score), Einkommen (income_annum), 
                        Kredithöhe (loan_amount) und Kreditlaufzeit (loan_term) 
                        die wichtigsten Einflussfaktoren. Die stark grünen Daten 
                        weisen auf eine nidrige Werte des Merkmals hin,
                        wohingegen die gelben Datenpunkte auf hohe Werte hindeuten.
                        Aus den Abbildung lässt sich ableiten, dass eine Person
                        mit einem hohen Kreditscore und einem geringeren Einkommen, 
                        gepaart mit einem hohen Kreditbetrag und kurzer Laufzeit 
                        sehr wahrscheinlich einen Kredit erhält.",
                        style = "font-size: 18px;"
                      ),
                    )
                  ),
                  fluidRow(
                    column(
                      2,
                      switchInput(
                        "shap.nn.slider",
                        size = "mini",
                        onStatus=T,
                        offStatus = F,
                        onLabel = "Train",
                        offLabel = "Test",
                        value=F
                      )
                    ),
                    column(
                      8,
                      plotlyOutput("shap.nn.ind")
                    ),
                  ),
                  fluidRow(
                    column(2),
                    column(
                      8,
                      tags$div(
                        h3("Abhängigkeits und Interaktionsdiagramm"),
                        "Die zweite Grafik ist für die logistische Regression 
                        nicht sinnvoll, denn die Shaple Werte eines Merkmals 
                        verlaufen Proportional zu der Ausprägung des Merkmals. 
                        Dieser Zusammenhang ist mathamtisch begründet.
                        Die Grafik ist dennoch wichtig, um den Zusammenhang zwischen 
                        der logistischen Regression und den nicht linearen 
                        Modellen xgBoost und den  neuronalen Netzen zu verstehen.",
                        style = "font-size: 18px;"
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      2,
                      selectInput(
                        inputId = "shap.nn.slid.txt1",
                        label = "Wähle Feature Abzisse",
                        choices = colnames(loan_data)[c(-1,-12)],
                        selected = "cibil_score"
                      ),
                      selectInput(
                        inputId = "shap.nn.slid.txt2",
                        label = "Wähle SHAP Ordinate",
                        choices = colnames(loan_data)[c(-1,-12)],
                        selected = "cibil_score"
                      )
                    ),
                    column(
                      10,
                      plotlyOutput("shap.nn.depend")
                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      h1("Individuele Prognose"),
                      fluidRow(
                        column(
                          5,
                          uiOutput("txt.nn"),
                          br(),
                          sliderInput(
                            "user00.nn",
                            label = "Wähle individuellen User:",
                            min = 1,
                            max = 4269,
                            value = 3540,
                            step = 1,
                            width = "100%",
                            animate = T
                          ),
                          plotlyOutput("shap.nn.idividuell"),
                        ),
                        column(
                          5,
                          DTOutput("User.data.nn")
                        )
                      )
                    )
                  ),
                  fluidRow(
                    column(2),
                    column(
                      8,
                      h1("Heatmap Shapley Userdaten"),
                      sliderInput(
                        "range.nn",
                        "Bereich",
                        min=100, 
                        max=nrow(loan_data),
                        step=10,
                        value = 100,
                        width="100%",
                        animate = T
                        ),
                      plotlyOutput("heat.shap.nn")
                    )
                  )
                ),
                ## Xgboost ####
                tabItem(
                  tabName = "shap_xg",
                  tags$head(
                    tags$style(
                      HTML(".centered { text-align: center; }")
                    )
                  ) %>%
                    h1("SHAP-Abhängigkeits- und Interaktionsdiagramme", class = "centered"),
                  fluidRow(
                    column(2),
                    column(
                      8,
                      tags$div(
                        h3("Featureimportance"),
                        "In der ersten Grafik sind die Merkmale nach Wichtigkeit
                        sortiert. Wie bereits bei der Modellierung der logistischen 
                        Regression festgestellt wurde, sind die vier Merkmale 
                        Kredit Score (cibil score), Einkommen (income_annum), 
                        Kredithöhe (loan_amount) und Kreditlaufzeit (loan_term) 
                        die wichtigsten Einflussfaktoren. Die stark grünen Daten 
                        weisen auf eine nidrige Werte des Merkmals hin,
                        wohingegen die gelben Datenpunkte auf hohe Werte hindeuten.
                        Aus den Abbildung lässt sich ableiten, dass eine Person
                        mit einem hohen Kreditscore und einem geringeren Einkommen, 
                        gepaart mit einem hohen Kreditbetrag und kurzer Laufzeit 
                        sehr wahrscheinlich einen Kredit erhält.",
                        style = "font-size: 18px;"
                      ),
                    )
                  ),
                  fluidRow(
                    column(
                      2,
                      "Daten:",
                      switchInput(
                        inputId = "shap.xg.slider",
                        size = "mini",
                        onStatus=T,
                        offStatus = F,
                        onLabel = "Train",
                        offLabel = "Test",
                        value=F
                      )),
                    column(
                      8,
                      plotlyOutput("shap.xg.ind")
                    )
                  ),
                  fluidRow(
                    column(2),
                    column(
                      8,
                      div(
                        h3("Abhängigkeits und Interaktionsdiagramm"),
                        "Die zweite Grafik ist für die logistische Regression nicht
                        sinnvoll, denn die Shaple Werte eines Merkmals verlaufen 
                        Proportional zu der Ausprägung des Merkmals. Dieser 
                        Zusammenhang ist mathamtisch begründet.
                        Die Grafik ist dennoch wichtig, um den Zusammenhang zwischen 
                        der logistischen Regression und den nicht linearen Modellen 
                        xgBoost und den neuronalen Netzen zu verstehen.",
                        style = "font-size: 18px;"
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      2,
                      selectInput(
                        inputId = "shap.xg.slid.txt1",
                        label = "Wähle Feature Abzisse",
                        choices = colnames(loan_data)[c(-1,-12)],
                        selected = "cibil_score"
                      ),
                      selectInput(
                        inputId = "shap.xg.slid.txt2",
                        label = "Wähle SHAP Ordinate",
                        choices = colnames(loan_data)[c(-1,-12)],
                        selected = "cibil_score"
                      )
                    ),
                    column(
                      10,
                      plotlyOutput("shap.xg.depend"),
                    )
                  ),
                  fluidRow(
                    column(
                      12,
                      h1("Individuele Prognose"),
                      fluidRow(
                        column(
                          5,
                          uiOutput("txt.xg"),
                          br(),
                          sliderInput(
                            "user00.xg",
                            label = "Wähle individuellen User:",
                            min = 1,
                            max = 4269,
                            value = 3540,
                            step = 1,
                            width = "100%",
                            animate = T
                          ),
                          plotlyOutput("shap.xg.idividuell")
                        ),
                        column(
                          5,
                          DTOutput("User.data.xg")
                        )
                      )
                    )
                  ),
                  fluidRow(
                    column(2),
                    column(
                      8,
                      h1("Heatmap Shapley Userdaten"),
                      sliderInput(
                        "range.xg",
                        "Bereich",
                        min=100, 
                        max=nrow(loan_data),
                        step=10,
                        value = 100,
                        width="100%",
                        animate = T
                        ),
                      plotlyOutput("heat.shap.xg")
                    )
                  )
                ),
                tabItem(
                  tabName = "shap_shap",
                  fluidRow(
                    column(2),
                    column(
                      8,
                      div(
                        class = "bank-header",
                        h1(class = "bank-name", "Master Bank"),
                        div(
                          class = "bank-slogan", 
                          "Große Kredite, kleines Einkommen, minimales Bankvermögen.",
                          span(class = "asterisk", "*")
                        ),
                        span(class = "note", "*Bei guten Kreditscore"),
                        actionButton(
                          "btnSimu", 
                          "Kreditzusage in Sekunden!", 
                          class = "simulation-btn"
                        )
                      )
                    )
                  ),
                  fluidRow(
                    column(2),
                    column(
                      8,
                      uiOutput("simulation"),
                      br()
                    )
                  ),
                  fluidRow(
                    column(6,plotlyOutput("sim.p2")),
                    column(6,plotlyOutput("sim.p1"))
                  ),
                  fluidRow(
                    column(1),
                    column(
                      10,
                      uiOutput("titleSlider"),
                      uiOutput("sim.s2"),
                      box(
                        width = 6,
                        uiOutput("sim.reload"),
                        uiOutput("sim.s9"),
                        uiOutput("sim.s10"),
                        uiOutput("sim.s1"),
                        uiOutput("sim.s3"),
                        uiOutput("sim.s4")
                      ),
                      box(
                        width = 6,
                        uiOutput("sim.s5"),
                        uiOutput("sim.s6"),
                        uiOutput("sim.s7"),
                        uiOutput("sim.s8"),
                        uiOutput("sim.s11")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

