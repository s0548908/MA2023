packages <- c(
  "fastDummies", 
  "tidyverse",
  "kableExtra","
  visNetwork",
  "gridExtra",
  "keras",
  "tensorflow",
  "ggplot2",
  "xgboost",
  "SHAPforxgboost",
  "caret"
)

for (package in packages) {
  if (!require(package)) {
    install.packages(package)
  }
}
library(fastDummies)
library(tidyverse)
library(kableExtra)
library(visNetwork)
library(gridExtra)
library(keras)
library(tensorflow)
library(ggplot2)
library(xgboost)
library(SHAPforxgboost)
loadfonts(device = "pdf")
# Daten laden und Dummy Codierung ####
source("~/GitHub/MA2023/R/Datenvorbereitung.R")

# Modellierung LR ####
source("~/GitHub/MA2023/R/fit_LR.R")
kable(
  tbl,
  format = "pipe",
  booktabs = TRUE,
  caption = "Vergleich Beta Parameter logistische Regression"
)
summary(lr)$coefficients %>%
  kable(
    format = "pipe",
    booktabs = TRUE,
    caption = "Zusammenfassung logistische Regression",
    label = "lrSummary",
    digits=3
  )

## VisNetwork LR ####
source("~/GitHub/MA2023/R/NetzgrafikLR.R")

## Konfusionsplot #### 
source("~/GitHub/MA2023/R/create_confusion_plot.R")
confusion_plot(
  pred_train = pred.round.train.lr,
  pred_test = pred.round.test.lr,
  plot_title = "Konfusionsdaten für Trainings- und Testdaten - Logistische Regression"
  )

# Modelliergun NN ####
source("~/GitHub/MA2023/R/fit_nn.R")

## Netzgrafik nn ####
source("~/GitHub/MA2023/R/NetzgrafikNN.R")
## Konfusionsplot ####
confusion_plot(
  pred.round.train.nn,
  pred.round.test.nn,
  "Konfusionsdaten für Trainings- und Testdaten - Neuronales Netz"
)

# xgboost ####
source("~/GitHub/MA2023/R/fit_xg.R")
## Konfusionsplot ####
confusion_plot(
  pred.round.train.xg,
  pred.round.test.xg,
  "Konfusionsdaten für Trainings- und Testdaten - xgBoost"
)

## Erster Baum ####
xgb.plot.tree(model = xgb_mod.s, trees = 0, show_node_id = TRUE)

# SHAPley Values ####
source("~/GitHub/MA2023/R/ShapleyValues_LR.R")
source("~/GitHub/MA2023/R/ShapleyValues_NN.R")
df<-data.frame(
  Vorhersage=pred.exakt.train.nn[1:5],
  "Summe SHAP + BIAS"= c(
    sum(shap.train.nn[1,]),
    sum(shap.train.nn[2,]),
    sum(shap.train.nn[3,]),
    sum(shap.train.nn[4,]),
    sum(shap.train.nn[5,]))
)
source("~/GitHub/MA2023/R/ShapleyValues_XG.R")

## Vergleich Summen PLot ####
source("~/GitHub/MA2023/R/SummenShapleyPlot.R")
df_xg <- data.frame(
  pred = pred.round.test.xg,
  shap = shap.xg.sum.test,
  Richtig = pred.round.test.xg == norm.test$loan_status
)
df_nn <- data.frame(
  pred = pred.round.test.nn,
  shap = shap.nn.sum.test,
  Richtig = pred.round.test.nn == norm.test$loan_status
)

df_lr <- data.frame(
  pred = pred.round.test.lr,
  shap = shap.lr.sum.test,
  Richtig = pred.round.test.lr == norm.test$loan_status
)
p1 <- SummenShapleyPlot(df_xg, "XgBoost", "xg-Vorhersage",T)
p2 <- SummenShapleyPlot(df_nn, "Neuronales Netz", "nn-Vorhersage",F)
p3 <- SummenShapleyPlot(df_lr, "Logistische Regression", "lr-Vorhersage",F)
grid.arrange(p3, p2, p1, ncol = 3)

## FeatureImportance ####
source("~/GitHub/MA2023/R/FeatureimportancePlot.R")
featureImportance.Plot(shap.train.lr,"Logistische Regression Featureimportance")
featureImportance.Plot(shap.train.xg,"XgBoost Featureimportance")
featureImportance.Plot(shap.train.nn,"Neuronales Netz Featureimportance")

## Abhängigkeitsplot ####
source("~/GitHub/MA2023/R/Abhängigkeitsplot.R")
Abhängigkeitsplot(shap.train.lr,pred.round.train.lr,"cibil_score","Logistische Regression")
Abhängigkeitsplot(shap.train.nn,pred.round.train.nn,"cibil_score","Neuronales Netz")
Abhängigkeitsplot(shap.train.xg,pred.round.train.xg,"cibil_score","XgBoost")

## Individueller Plot ####
source("~/GitHub/MA2023/R/individualPlot.R")
individualPlot(shapData = shap.train.lr,id = 125,referenzData = tbltrain)
individualPlot(shapData = shap.train.xg,id = 5,referenzData = tbltrain)
individualPlot(shapData = shap.train.nn,id = 2182,referenzData = tbltrain)
individualPlot(shapData = shap.test.lr,id = 283,referenzData = tbltest,F)
individualPlot(shapData = shap.test.xg,id = 283,referenzData = tbltest,F)
individualPlot(shapData = shap.test.nn,id = 283,referenzData = tbltest,T)
norm.test$loan_status[283]
tbltest$loan_status[283]
df_xg <- data.frame(
  pred = pred.round.train.xg,
  shap = shap.xg.sum.train,
  Richtig = pred.round.train.xg == norm.train$loan_status
)
# Format für die Bezeichnung des Waterfall Plots
f <- function(x) format(x, big.mark = ".", decimal.mark =",", scientific = FALSE)

shapData<- shap.test.xg
id <- 125
referenzData<-tbltest
individualPlot<-function(shapData,id,referenzData){
  # Daten neu strukturieren
  data <- data.frame(
    variable = colnames(shapData[c(12,1:11)]), 
    value = as.numeric(shapData[id,c(12,1:11)])
  )
  data$variable <- factor(data$variable, levels = rev(data$variable))
  
  # Plot 1 Balkendiagram
  p1<-ggplot(data, aes(x = value, y = variable, fill = value > 0)) + 
    geom_col() +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
    labs(
      title = "Individual SHAP Values USER 125",
      x = "Variable",
      y = "SHAP Value"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      legend.position = "none"
    ) +
    coord_cartesian(ylim = c(-1.5, NA))
  
  # Kummulierte Daten berechnen
  features <- colnames(shapData)[c(12, 1:11)]
  cumulative_values <- cumsum(shapData[id, c(12, 1:11)] %>% as.vector())
  cumulative_values<-1/(1+exp(-cumulative_values))
  
  # Kreise vorbereiten
  arrow_x <- cumulative_values
  arrow_y <- 12:1
  base_x <- c(.5, cumulative_values) %>% unlist()
  base_y <- 12:0
  
  # Achsen beschriftung vorbereiten
  tbl <- tibble(
    a = c(min(cumulative_values), max(cumulative_values) , cumulative_values[12]),
    b = c(min(cumulative_values) %>% round(1), 
          max(cumulative_values) %>% round(1),
          paste("Endwert", round(cumulative_values[12], 2))
    )
  ) %>% 
    arrange(a)
  # Waterfall Plot
ggplot() +
    # Horizontale Linien
    annotate(
      "segment",
      x = base_x[-length(base_x)],
      xend = base_x[-1],
      y = base_y[-12],
      yend = base_y[-12],
      arrow = arrow(type = "closed", length = unit(0.5, "inches")),
      color = "black", size = 1.5
    )+
  
    # Vertikale Linien
    # geom_segment(
    #   aes(
    #     x = base_x[-1], 
    #     xend = base_x[-1],
    #     y = base_y[-length(base_y)], 
    #     yend = base_y[-1]), 
    #   color = "black", 
    #   size = 1.5) +
    geom_point(
      data = data.frame(
        x = arrow_x, 
        y = arrow_y, 
        fill = ifelse(diff(c(base_x)) > 0, "green", "red")
      ), 
      aes(x = x, y = y, fill = fill), 
      shape = 21, 
      size = 5) +
    scale_fill_identity() +
    scale_x_continuous(
      name = "Prognosewert", 
      breaks = tbl$a, 
      labels = tbl$b
    ) +
    scale_y_continuous(
      name = "", 
      breaks = 12:0, 
      labels = paste(
        c(names(base_x)[2:13],"Endwert"),
        "\n=",
        c(cumulative_values[1]
          %>%round(3),
          referenzData[id,2:12] 
          %>%f(),
          cumulative_values[12] %>%
            round(3)
        )
      )
    ) +
    labs(title = "Kummulierte \n Shapley Values") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank()
    )
  
  grid.arrange(p1, p2, ncol = 2)
}

features <- colnames(shapData)[c(12, 1:11)]
cumulative_values <- cumsum(shapData[id, c(12, 1:11)] %>% as.vector())
cumulative_values<-1/(1+exp(-cumulative_values))

# Kreise vorbereiten
arrow_x <- cumulative_values
arrow_y <- 12:1
base_x <- c(.5, cumulative_values) %>% unlist()
base_y <- 12:0

# Achsen beschriftung vorbereiten
tbl <- tibble(
  a = c(min(cumulative_values), max(cumulative_values) , cumulative_values[12]),
  b = c(min(cumulative_values) %>% round(1), 
        max(cumulative_values) %>% round(1),
        paste("Endwert", round(cumulative_values[12], 2))
  )
) %>% 
  arrange(a)

text_data<-shap.train.xg[125,] %>% round(3) 
names(shap.train.xg[125,])
ggplot() +
  annotate(
    "segment",
    x = base_x[-length(base_x)],
    xend = base_x[-1],
    y = base_y[-13],
    yend = base_y[-13],
    arrow = arrow(type = "closed", length = unit(0.05, "inches")),
    color = ifelse(diff(c(base_x)) > 0, "green", "red"),
    size = 2  # Hier die Linienstärke anpassen
  )+
  geom_text(
    aes(
      x = base_x[-length(base_x)], 
      y = base_y[-13]+.2, 
      label =
        ifelse(
          diff(c(base_x))>0,
          paste("+", diff(c(base_x)) %>% round(3) %>% as.character()),
          diff(c(base_x)) %>% round(3) %>% as.character()
        )
      ),
    size=3.5,
    color="gray"
  )+
  scale_x_continuous(
    name = "Prognosewert", 
    breaks = tbl$a, 
    labels = tbl$b
  ) +
  scale_y_continuous(
    name = "", 
    breaks = 12:0, 
    labels = paste(
      c(names(base_x)[2:13],"Endwert"),
      "\n=",
      c(cumulative_values[1]
        %>%round(3),
        referenzData[id,2:12] 
        %>%f(),
        cumulative_values[12] %>%
          round(3)
      )
    )
  ) +
  labs(title = "Kummulierte \n Shapley Values") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()
  )

