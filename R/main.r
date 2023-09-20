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
  "caret",
  "DiagrammeR"
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
data.frame(
  Vorhersage=pred.exakt.train.nn[1:5],
  "Summe SHAP + BIAS"= c(
    1/(1+exp(-sum(shap.train.nn[1,]))),
    1/(1+exp(-sum(shap.train.nn[2,]))),
    1/(1+exp(-sum(shap.train.nn[3,]))),
    1/(1+exp(-sum(shap.train.nn[4,]))),
    1/(1+exp(-sum(shap.train.nn[5,]))))
) %>%
  kable(format = "pipe")
source("~/GitHub/MA2023/R/ShapleyValues_XG.R")

## Vergleich Summen PLot ####
source("~/GitHub/MA2023/R/SummenShapleyPlot.R")
df_xg <- data.frame(
  pred = pred.round.test.xg,
  shap = 1/(1+exp(-shap.xg.sum.test)),
  Richtig = pred.round.test.xg == norm.test$loan_status
)
df_nn <- data.frame(
  pred = pred.round.test.nn,
  shap = 1/(1+exp(-shap.nn.sum.test)),
  Richtig = pred.round.test.nn == norm.test$loan_status
)

df_lr <- data.frame(
  pred = pred.round.test.lr,
  shap = 1/(1+exp(-shap.lr.sum.test)),
  Richtig = pred.round.test.lr == norm.test$loan_status
)
p1 <- SummenShapleyPlot(df_xg, "XgBoost", "xg-Vorhersage",T)
p2 <- SummenShapleyPlot(df_nn, "Neuronales Netz", "nn-Vorhersage",F)
p3 <- SummenShapleyPlot(df_lr, "Logistische Regression", "lr-Vorhersage",F)
grid.arrange(p3, p2, p1, ncol = 3)
plot(pred.round.test.nn,1/(1+exp(-shap.nn.sum.test)))
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
individualPlot(shapData = shap.train.lr,id = 125,referenzData = tbltrain,nn = F)
individualPlot(shapData = shap.train.xg,id = 5,referenzData = tbltrain,nn = F)
individualPlot(shapData = shap.train.nn,id = 2182,referenzData = tbltrain,nn = F)
individualPlot(shapData = shap.test.lr,id = 125,referenzData = tbltest,F)
individualPlot(shapData = shap.test.xg,id = 125,referenzData = tbltest,F)
individualPlot(shapData = shap.test.nn,id = 125,referenzData = tbltest,F)


