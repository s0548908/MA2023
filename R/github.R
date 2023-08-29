library(fastDummies)
library(tidyverse)
loan_data <- read.csv(
  "~/MA/Masterarbeit/data/loan_approval_dataset.csv",
  stringsAsFactors = T)

loan_data<-loan_data %>%
  dummy_cols(
    remove_selected_columns = T,remove_first_dummy = F
  )  %>%
  dplyr::select(
    -"self_employed_ Yes",
    -"education_ Not Graduate",
    -"loan_status_ Rejected")
colnames(loan_data)[13]<-"loan_status"
colnames(loan_data)[11]<-"education_Graduate"
colnames(loan_data)[12]<-"self_employed_No"

# Code 4.2 (Aufteilung Trainings- und Testdaten).
set.seed(123)
idx<-sample(nrow(loan_data),nrow(loan_data)*.8,F)
tbltrain<-loan_data[idx,]
tbltest<-loan_data[-idx,]

# Funktion zur Normalisierung
normalize <- function(x, min_values, max_valvalues) {
  return((x - min_values) / (max_valvalues - min_values))
}

# Min und Max Werte des Trainingsdatensatzes berechnen
min_vals <- sapply(tbltrain %>% dplyr::select(-loan_id), min)
max_vals <- sapply(tbltrain %>% dplyr::select(-loan_id), max)

# Trainingsdatensatz normalisieren
norm.train <- as.data.frame(
  mapply(
    function(x, min_val, max_valvalues) 
      normalize(x, min_val, max_valvalues), 
    tbltrain %>% dplyr::select(-loan_id), 
    min_vals, 
    max_vals, 
    SIMPLIFY = FALSE)
  )
# Testdatensatz normalisieren mit den Min und Max Werten des Trainingsdatensatzes
norm.test <- as.data.frame(
  mapply(
    function(x, min_val, max_val) 
      normalize(x, min_val, max_val), 
    tbltest %>% dplyr::select(-loan_id), 
    min_vals, 
    max_vals, 
    SIMPLIFY = FALSE)
)
# loan_status als Faktor setzen
norm.train$loan_status <- as.factor(tbltrain$loan_status)
norm.test$loan_status <- as.factor(tbltest$loan_status)




#Code 4.3 (Modellierung Logistische Regression).
lr <- glm(loan_status ~ ., data=norm.train, family="binomial")

# Code 4.4 (Modellierung Beta-Koeffizienten mittel Log-Likelihood).
library(kableExtra)
# Factor umwandeln on 0 | 1
norm.train$loan_status<-as.numeric(norm.train$loan_status)-1
# Log-Likelihood
logLik <- function(beta, y, X) {
  p <- 1 / (1 + exp(-(beta[1] + X %*% beta[-1])))
  sum(y*log(p) + (1-y)*log(1-p))
}

start <- rep(0, 12)
res <- optim(
  start,
  function(par) -logLik(
    par,
    norm.train$loan_status,
    norm.train[,-12] %>%
      as.matrix()),
  method="BFGS"
)
tbl<-tibble(
  Feature = names(lr$coefficients),
  beta.glm=lr$coefficients %>% round(5),
  beta.manuel=res$par%>% round(5)
)
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

# VisNetwork
library(visNetwork)
edges<-tibble(
  from=1:24,
  to = c(12:22,rep(24,12),25),
  color="#8105f5",
  font.color="#8105f5",
  label  = c(rep("*",11),rep("",12),"sigmoid")
)
nodes<-tibble(
  id=1:25,
  levels=c(rep(1,11),rep(2,12),3,4),
  label = c(
    paste(
      "Eingang \n",
      names(norm.train[1:11])
      ),
    paste(
      "beta",1:11,"\n",
      names(norm.train[1:11]),"\n",
      lr$coefficients[c(2:12)] %>% 
        round(digits = 4)
    ),
    paste(
      "Intercept\n",      
      lr$coefficients[c(1)] %>% 
        round(digits = 4)
      ),
    "∑",
    "Ziel/Ausgang"
  ),
  shape=c(
    rep("box",11),
    rep("dot",12),
    "box",
    "star"),
  size =c(
    rep(25,11),
    lr$coefficients[c(2:12,1)] %>% 
      abs(),
    rep(25,2) 
    ),
  color=c(
    rep("orange",11),
    rep("#922ef0",13),
    "orange"
    )
)
visNetwork(
  nodes, 
  edges,width = "100%", 
  height = "1100px"
  ) %>%
  visEdges(arrows = "to") %>%
  visNodes(
    font = list(color = "blue"), 
    size = nodes$size
    ) %>%
  visOptions(
    highlightNearest = list(
      enabled = T, 
      degree = 1, 
      hover = T
      ), 
    nodesIdSelection = F
    ) %>%
  visLayout(
    randomSeed = 14, 
    improvedLayout = TRUE,
    hierarchical = list(
      enabled = F, 
      direction = 'DU', 
      sortMethod = 'directed')
    ) %>% 
  visPhysics(
    stabilization = list(
      enabled = TRUE, 
      iterations = 10
      ),
    solver = "forceAtlas2Based",
    forceAtlas2Based = list(
      gravityConstant = 0,
      springLength = 200,
      springConstant = 0.1
      )
    )  

logit<-function(x) 1/(1+exp(-x))
pred.exakt.train.lr<-(sweep(norm.train[,-12], 2, lr$coefficients[2:12], "*") %>%
  rowSums()+lr$coefficients[1]) %>%
  logit()
pred.exakt.test.lr<-(sweep(norm.test[,-12], 2, lr$coefficients[2:12], "*") %>%
  rowSums()+lr$coefficients[1]) %>%
  logit()

pred.round.train.lr<-pred.exakt.train.lr %>% round()
pred.round.test.lr<-pred.exakt.test.lr %>% round()

# Erstellen Sie die Testdaten-Konfusionsmatrix
# Erstellen der Konfusionsmatrizen
create_confusion_plot <- function(pred_train, pred_test, plot_title) {
  
df_train <- caret::confusionMatrix(
  pred_train %>%
    factor(),
  norm.train$loan_status %>%
    factor()
)$table %>%
  as.data.frame()
df_test <- caret::confusionMatrix(
  pred_test %>%
    factor(),
  norm.test$loan_status %>%
    factor()
)$table %>%
  as.data.frame()
# Daten zusammenfügen und Labels hinzufügen
df <- rbind(
  transform(
    df_train, 
    dataset = "Trainingsdaten", 
    label = paste(c("TN", "FN", "FP", "TP"), Freq)
    ),
  transform(
    df_test, 
    dataset = "Testdaten", 
    label = paste(c("TN", "FN", "FP", "TP"), Freq)
    )
)

# Levels und Labels setzen
levels(df$Prediction) <- c("Abgelehnt", "Genehmig")
levels(df$Reference) <- c("Abgelehnt", "Genehmig")

# Plot erstellen
ggplot(
  df, 
  aes(x = Reference, y = Freq, fill = Prediction)
  ) +
  geom_bar(
    stat = "identity", 
    position = "stack"
    ) +
  geom_text(
    aes(label = label), 
    position = position_stack(vjust = 0.5)
    ) +
  facet_wrap(
    ~dataset, 
    scales = "free_y"
    ) +
  labs(
    x = "Vorhersage",
    y = "Anzahl",
    fill = "Status",
    title = plot_title
  ) +
  theme_minimal()
}
create_confusion_plot(
  pred.round.train.lr,
  pred.round.test.lr,
  "Konfusionsdaten für Trainings- und Testdaten - Logistische Regression"
  )

library(keras)
library(tensorflow)
fit.nn <- function() {
  nn <- keras_model_sequential()
  nn <- nn %>%
    layer_dense(units = 16, activation = 'relu', input_shape = 11) %>%
    layer_dense(units = 16, activation = 'relu') %>%
    layer_dense(units = 16, activation = 'relu') %>%
    layer_dense(units = 1, activation = 'sigmoid')
  nn %>% compile(
    loss = 'binary_crossentropy',
    optimizer = optimizer_adam(learning_rate = 0.001),
    metrics = c('accuracy')
  )
  history <- nn %>%
    fit(
      norm.train %>% select(-loan_status) %>% as.matrix(),
      norm.train$loan_status,
      epochs = 75,
      batch_size = 32,
      validation_split = 0.1,
    )
  return(list(nn,history))
}
results <- lapply(1:15, function(i) fit.nn())
nn.fits <- lapply(results, `[[`, 1)
metrics.list <- lapply(results, `[[`, 2)

# Netzgrafik
weights.list <- lapply(nn.fits, function(model) {
  model$get_weights()
})
edges<-tibble(
  from=c(rep(1:43, each = 16),44:59),
  to=c(
    rep(12:27, times=11),
    rep(28:43, times = 16),
    rep(44:59, times = 16), 
    rep(60, times = 16)
    ),
  label=weights.list[[1]][c(1,3,5,7)] %>% 
    unlist() %>% 
    round(4) %>% 
    as.character()
)
edges$font.color <- "orange"
edges$color <- "#8105f5"
edges$color[edges$from %in% 1:11] <- "orange"
edges$color[177:433]<-"#b11111"
edges$color[434:690]<-"green"
weights<-weights.list[[1]][c(2,4,6,8)] %>% 
  unlist()
nodes <- data.frame(
  id = 1:60, 
  label = paste(
    1:60,"Neuron"," 
    Bias:",
    c(rep(NA,11),round(weights,2))
    )
  )
nodes$group<-c(
  rep("Eingangsschicht",11),
  rep("Layer 1",16),
  rep("Layer 2",16),
  rep("Layer 3",16),
  "Ausgangsschicht"
  )

nodes$shape<-c(
  rep("box",11),
  rep("dot",16),
  rep("dot",16),
  rep("dot",16),
  "star")
nodes$color<-c(
  rep("orange",11),
  rep("#b11111",16),
  rep("green",16),
  rep("#8105f5",16),
  "orange")

visNetwork(nodes, edges,width = "100%", height = "800px") %>%
  visGroups(groupname = "Ausgangsschicht", color = "orange",shape="star") %>%
  visGroups(groupname = "Layer 3", color = "#8105f5",shape="dot") %>%
  visGroups(groupname = "Layer 2", color = "green",shape="dot") %>%
  visGroups(groupname = "Layer 1", color = "#b11111",shape="dot") %>%
  visGroups(groupname = "Eingangsschicht", color = "orange",shape="box") %>%
  visLegend() %>%
  visLayout(
    randomSeed = 3,
    improvedLayout = TRUE
    ) %>% 
  visEdges(arrows = "to") %>%
  visNodes(font = list(color = "black")) %>%
  visOptions(
    highlightNearest = list(
      enabled = T, 
      degree = 0,
      hover=T), 
    nodesIdSelection = F) %>%
  visPhysics(
    stabilization = list(
      enabled = TRUE, 
      iterations = 100
      ),
    solver = "forceAtlas2Based",
    forceAtlas2Based = list(
      gravityConstant = 0,
      springLength = 50,
      springConstant = 0.01)
  )

# 
library(gridExtra)
library(ggplot2)
library(gridExtra)

# Funktion zur Datenzusammenführung
combine_data <- function(metrics.list, metric1, metric2) {
  map2_df(
    1:length(metrics.list), 
    metrics.list, ~ {
      history_data <- .y$metrics
      epochs <- 1:length(history_data[[metric1]])
      df_metric1 <- data.frame(
        epochs = epochs, 
        value = history_data[[metric1]], 
        metric = metric1, 
        model = paste0("Model ", .x)
      )
      df_metric2 <- data.frame(
        epochs = epochs, 
        value = history_data[[metric2]], 
        metric = metric2, 
        model = paste0("Model ", .x)
      )
      rbind(df_metric1, df_metric2)
    }
  )
}

# Daten für beide Plots zusammenführen
data_loss <- combine_data(metrics.list, "loss", "val_loss")
data_accuracy <- combine_data(metrics.list, "accuracy", "val_accuracy")

# Plot-Funktion
create.plot <- function(data, title, colors) {
  ggplot(
    data, 
    aes(x = epochs, y = value, color = metric, group = interaction(metric, model))
    ) +
    geom_line() +
    scale_color_manual(values = colors) +
    theme_minimal() +
    labs(title = title, x = "Epoche", y = "Wert")
}

# Plots erstellen
p1 <- create.plot(
  data_loss, 
  "Loss über Epochen", 
  c("loss" = "red", "val_loss" = "blue")
  )
p2 <- create.plot(
  data_accuracy, 
  "Accuracy über Epochen", 
  c("accuracy" = "red", "val_accuracy" = "blue")
  )

# Plots nebeneinander anzeigen
grid.arrange(p1, p2, ncol=2)


#
nn.get.predictions <- function(nn, data) {
  predictions <- nn %>% 
    predict(data)
  return(predictions)
}
# Trainingsdaten
nn.all.predictions.train <- lapply(
  nn.fits, 
  nn.get.predictions, 
  data = norm.train %>% 
    select(-loan_status) %>% 
    as.matrix()
  )
pred.exakt.train.nn <- rowMeans(do.call(cbind, nn.all.predictions.train)) 
pred.round.train.nn <- pred.exakt.train.nn %>% 
  round()
nn.all.predictions.test <- lapply(
  nn.fits,
  nn.get.predictions, 
  data = norm.test %>% 
    select(-loan_status) %>% 
    as.matrix()
  )
pred.exakt.test.nn <- rowMeans(do.call(cbind, nn.all.predictions.test)) 

pred.round.test.nn <- pred.exakt.test.nn %>% 
  round()
create_confusion_plot(
  pred.round.train.nn,
  pred.round.test.nn,
  "Konfusionsdaten für Trainings- und Testdaten - Neuronales Netz"
)


# xgboost
library(xgboost)
norm.train$loan_status <- norm.train$loan_status %>%
  as.character() %>%
  as.numeric()
norm.test$loan_status <- norm.test$loan_status %>%
  as.character() %>%
  as.numeric()
xgtrain <- xgb.DMatrix(
  norm.train %>% 
    dplyr::select(-loan_status) %>%
    as.matrix(),
  label=norm.train$loan_status
)
xgtest <- xgb.DMatrix(
  norm.test %>% 
    dplyr::select(-loan_status) %>%
    as.matrix(),
  label=norm.test$loan_status
)
set.seed(123)
param <- list(
  objective = "binary:logistic", 
  booster = "gbtree",            
  eta = 0.01,                    
  gamma = 0.1,                   
  max_depth = 6,       
  min_child_weight = 1,        
  subsample = 0.8,         
  colsample_bytree = 0.8,  
  eval_metric = "error"         
)
watchlist <- list(train = xgtrain, valid = xgtest)
set.seed(123)
xgb_mod.s <- xgb.train(
  data = xgtrain, 
  params=param, 
  nrounds = 50,
  watchlist = watchlist, 
  print_every_n = 1, 
  maximize = T,
  verbose = F,
  base_score =.5)
# Erster Baum
#xgb.plot.tree(model = xgb_mod.s, trees = 0, show_node_id = TRUE)

eval_log<-data.frame(xgb_mod.s$evaluation_log)

ggplot(eval_log, aes(x = iter)) +
  geom_line(aes(y = train_error, color = "Train Error"), size = 1) +
  geom_point(aes(y = train_error, color = "Train Error"), size = 3) +
  geom_line(aes(y = valid_error, color = "Validation Error"), size = 1) +
  geom_point(aes(y = valid_error, color = "Validation Error"), size = 3) +
  labs(title = "Training and Validation Error", x = "Iteration", y = "Error") +
  scale_color_manual(values = c("Train Error" = "#0d421f", "Validation Error" = "orange")) +
  theme_minimal()

pred.exakt.train.xg<-predict(xgb_mod.s,xgtrain)
pred.round.train.xg<-pred.exakt.train.xg %>% 
  round()
pred.exakt.test.xg<-predict(xgb_mod.s,xgtest)
pred.round.test.xg<- pred.exakt.test.xg %>%
  round()
create_confusion_plot(
  pred.round.train.xg,
  pred.round.test.xg,
  "Konfusionsdaten für Trainings- und Testdaten - xgBoost"
)


# SHAP LR
shap.train.lr<-(norm.train[,1:11] %>%
  as.matrix() %*% diag(lr$coefficients[2:12]) -
  matrix(
    lr$coefficients[2:12] * colMeans(norm.train[, 1:11]),
    nrow=nrow(norm.train),
    ncol=11,
    byrow=TRUE
  ))%>%
  data.frame()
shap.train.lr<-data.frame(shap.train.lr)
names(shap.train.lr)<-names(norm.train[-12])
shap.train.lr$BIAS<-shap.train.lr$BIAS<-predict(lr) %>%
  mean() 

shap.lr.sum.train<-rowSums(shap.train.lr)
shap.test.lr<-norm.test[,1:11] %>%
  as.matrix() %*% diag(lr$coefficients[2:12]) -
  matrix(
    lr$coefficients[2:12] * colMeans(norm.train[, 1:11]),
    nrow=nrow(norm.test),
    ncol=11,
    byrow=TRUE
  )
shap.test.lr<-data.frame(shap.test.lr)
names(shap.test.lr)<-names(norm.test[-12])
shap.test.lr$BIAS<-predict(lr,newdata = norm.test) %>%
  mean() 
shap.lr.sum.test<-rowSums(shap.test.lr)
# SHAP XG 
shap.train.xg <- predict(xgb_mod.s, xgtrain, predcontrib = TRUE) %>%
  data.frame()
shap.test.xg <- predict(xgb_mod.s, xgtest, predcontrib = TRUE) %>%
  data.frame()
shap.xg.sum.train<-rowSums(shap.train.xg)
shap.xg.sum.test<-rowSums(shap.test.xg)
# SHAP NN
shap <- reticulate::import('shap')
explainers <- lapply(nn.fits, function(model) {
  shap$DeepExplainer(
    model,
    norm.train %>%
      select(-loan_status) %>%
      as.matrix()
  )
})
shap_values_list_train <- lapply(explainers, function(explainer) {
  explainer$shap_values(
    norm.train %>%
      select(-loan_status) %>%
      as.matrix()
  )
})
shap_values_list_test <- lapply(explainers, function(explainer) {
  explainer$shap_values(
    norm.test %>%
      select(-loan_status) %>%
      as.matrix()
  )
})
shap.train.nn <- Reduce(`+`, lapply(shap_values_list_train, `[[`, 1)) /
  length(nn.fits)
shap.train.nn<-data.frame(shap.train.nn)
names(shap.train.nn)<-names(norm.train[-12])
shap.train.nn$BIAS<-mean(pred.exakt.train.nn)
shap.nn.sum.train<-rowSums(shap.train.nn)

shap.test.nn <- Reduce(`+`, lapply(shap_values_list_test, `[[`, 1)) /
  length(nn.fits)
shap.test.nn<-data.frame(shap.test.nn)
names(shap.test.nn)<-names(norm.test[-12])
shap.test.nn$BIAS<-mean(pred.exakt.test.nn)
shap.nn.sum.test<-rowSums(shap.test.nn)

# Vergleich Summen PLot
library(gridExtra)
df<-data.frame(
  pred.round.test.xg,
  shap.xg.sum.test,
  "Richtig"=pred.round.test.xg == norm.test$loan_status)
p1<-df %>%
  mutate(
    pred.round.test.xg = pred.round.test.xg + c( rnorm(length(pred.round.test.xg), 0, 0.015))
  ) %>%
  ggplot(aes(x = pred.round.test.xg , y = shap.xg.sum.test, color = Richtig)) +
  geom_point() +
  scale_color_manual(values = c("red", "black"))+
  labs(
    title = "XgBoost",
    x =  "xg-Vorhersage",
    y = "SHAP xg Sum"
  ) +
  scale_x_continuous(breaks = c(0, 1), labels = c("Abgelehnt", "Genehmigt")) +
  theme_minimal() 

df<-data.frame(
  pred.round.test.nn,
  shap.nn.sum.test,
  "Richtig"=pred.round.test.nn == norm.test$loan_status)

p2<-df %>%
  mutate(
    pred.round.test.nn = pred.round.test.nn + c( rnorm(length(pred.round.test.nn), 0, 0.015))
  ) %>%
  ggplot(aes(x = pred.round.test.nn , y = shap.nn.sum.test, color = Richtig)) +
  geom_point() +
  scale_color_manual(values = c("red", "black"))+
  labs(
    title = "Neuronales Netz",
    x =  "nn-Vorhersage",
    y = "SHAP nn Sum"  
  ) +
  scale_x_continuous(breaks = c(0, 1), labels = c("Abgelehnt", "Genehmigt")) +
  theme_minimal() +
  theme(legend.position = "none")



df<-data.frame(
  pred.round.test.lr,
  shap.lr.sum.test,
  "Richtig"=pred.round.test.lr == norm.test$loan_status)

p3<-df %>%
  mutate(
    pred.round.test.lr = pred.round.test.lr + c( rnorm(length(pred.round.test.lr), 0, 0.015))
  ) %>%
  ggplot(aes(x = pred.round.test.lr , y = shap.lr.sum.test, color = Richtig)) +
  scale_color_manual(values = c("red", "black"))+
  geom_point() +
  labs(
    title = "Neuronales Netz",
    x =  "lr-Vorhersage",
    y = "SHAP lr Sum"  
  ) +
  scale_x_continuous(breaks = c(0, 1), labels = c("Abgelehnt", "Genehmigt")) +
  theme_minimal() +
  theme(legend.position = "none")

grid.arrange(p3,p2,p1, ncol = 3)

# FeatureImportance
library(SHAPforxgboost)
featureImportance.Plot<-function(data,titel){
  shap.train.long<-shap.prep(
    shap_contrib = data[-12], 
    X_train = norm.train[-12])
  shap.train.long$y_values <- rnorm(nrow(shap.train.long), 0, 0.05)
  # Hinzufügen von y_values und Farben
  shap.train.long <- shap.train.long %>%
    group_by(variable) %>%
    mutate(
      correlation = cor(value, rfvalue),
      color = ifelse(correlation > 0, -value, value)
    ) %>%
    ungroup()
  # Erstellen von y-Achsen-Labels
  txt <- shap.train.long %>%
    group_by(variable) %>%
    summarise(mean = sprintf("%.3f", mean(abs(value))))
  
  ggplot(
    shap.train.long, 
    aes(x = value, y = y_values + as.numeric(factor(variable)), color = color)) +
    geom_point(size = 1) +
    scale_y_continuous(breaks = 1:11, labels = paste(txt$variable,txt$mean)) +
    scale_color_gradient(
      low = "orange", 
      high = "#0d421f", 
      name = "Merkmal \n(hoch =grün)") +
    labs(
      title = titel,
      x = "Shapley Values",
      y = "",
      color = "Merkmal") +
    theme_minimal() 
}
data<-shap.train.lr
featureImportance.Plot(shap.train.lr,"Logistische Regression Featureimportance")
featureImportance.Plot(shap.train.xg,"XgBoost Featureimportance")
featureImportance.Plot(shap.train.nn,"Neuronales Netz Featureimportance")


# depend
depend.cibil.plot<-function(data,pred,titel){
  loess_fit <- loess(data$cibil_score ~ norm.train$cibil_score)
  predicted_values <- predict(
    loess_fit, 
    newdata = data.frame(x = sort(norm.train$cibil_score))
  )
  
  cor.test.data<-cor.test(
    norm.train$cibil_score,
    data$cibil_score
  )
  cor_text <- paste0(
    "R = ", round(cor.test.data$estimate, 2),
    ", p < ", ifelse(
      round(cor.test.data$p.value, 6) == 0, 
      "2.2e-16", 
      round(cor.test.data$p.value, 4)
    )
  )
  
  filtered_data <- tibble(
    rfvalue = norm.train$cibil_score, 
    value = data$cibil_score, 
    richtig = pred == norm.train$loan_status, 
    loan_status = as.factor(norm.train$loan_status)
  )
  filtered_data$richtig <- factor(
    filtered_data$richtig, 
    levels = c(TRUE, FALSE), 
    labels = c("Richtige Vorhersage", "Falsche Vorhersage")
  )
  
  scatter <- ggplot(
    filtered_data, 
    aes(
      x = rfvalue, 
      y = value, 
      color = loan_status, 
      shape = richtig)
  ) +
    geom_point(aes(size = richtig)) +  
    geom_smooth(aes(group = 1), method = "loess", se = FALSE, color = "#0d421f")+
    scale_color_manual(values = c("red", "black")) +
    scale_shape_manual(values = c(16, 17)) +   
    scale_size_manual(
      values = c("Richtige Vorhersage" = 1, "Falsche Vorhersage" = 5)) +  
    labs(
      title=paste(cor_text,titel),
      x = "Referenz Wert",
      y = "Shapley Value",
      color = "Vorhersage"
    ) +
    theme_minimal()
  
  hist_x <- ggplot(filtered_data, aes(x = rfvalue)) +
    geom_histogram(
      aes(y = ..density..), 
      fill = "orange", 
      color = "#0d421f", 
      bins = 30) +
    theme_minimal()
  
  hist_y <- ggplot(filtered_data, aes(x = value)) +
    geom_histogram(
      aes(y = ..density..), 
      fill = "orange", 
      color = "#0d421f", 
      bins = 30) +
    coord_flip() +
    theme_minimal()
  
  grid.arrange(
    hist_x, 
    NULL, 
    scatter, 
    hist_y, 
    ncol = 2, 
    nrow = 2, 
    widths = c(4, 1), 
    heights = c(1, 4)
  )
}
depend.cibil.plot(shap.train.lr,pred.round.train.lr,"Logistische Regression")
depend.cibil.plot(shap.train.nn,pred.round.train.nn,"Neuronales Netz")
depend.cibil.plot(shap.train.xg,pred.round.train.xg,"XgBoost")
id<-125
f <- function(x) format(x, big.mark = ".", decimal.mark =",", scientific = FALSE)

waterBalken<-function(shapData,id){
  
  data <- data.frame(
    variable = c("BIAS",colnames(norm.test)[1:11]), 
    value = as.numeric(shapData[id,c(12,1:11)])
  )
  data$variable <- factor(data$variable, levels = rev(data$variable))
  
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
  
  
  features <- colnames(shapData)[c(12, 1:11)]
  cumulative_values <- cumsum(shapData[id, c(12, 1:11)] %>% as.vector())
  cumulative_values<-1/(1+exp(-cumulative_values))
  
  arrow_x <- cumulative_values
  arrow_y <- 12:1
  base_x <- c(.5, cumulative_values) %>% unlist()
  base_y <- 12:0
  
  tbl <- tibble(
    a = c(min(cumulative_values), max(cumulative_values) , cumulative_values[12]),
    b = c(min(cumulative_values) %>% round(1), 
          max(cumulative_values) %>% round(1),
          paste("Endwert", round(cumulative_values[12], 2))
    )
  ) %>% 
    arrange(a)
  
  p2<-ggplot() +
    # Horizontale Linien
    geom_segment(
      aes(
        x = base_x[-length(base_x)], 
        xend = base_x[-1], 
        y = base_y[-12], 
        yend = base_y[-12]
      ), 
      color = "black", size = 1.5) +
    # Vertikale Linien
    geom_segment(
      aes(
        x = base_x[-1], 
        xend = base_x[-1],
        y = base_y[-length(base_y)], 
        yend = base_y[-1]), 
      color = "black", 
      size = 1.5) +
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
          tbltest[125,2:12] 
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
waterBalken(shap.train.lr,125)
waterBalken(shap.test.nn,125)
waterBalken(shap.test.xg,125)
