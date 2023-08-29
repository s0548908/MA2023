# Modellierung neuronales Netz
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
# Extrahieren der Daten
results <- lapply(1:2, function(i) fit.nn())
nn.fits <- lapply(results, `[[`, 1)
metrics.list <- lapply(results, `[[`, 2)

# Vorhersagen berechnen
nn.get.predictions <- function(nn, data) {
  predictions <- nn %>% 
    predict(data)
  return(predictions)
}
# Trainingsdaten Vorhersagen (Matrix)
nn.all.predictions.train <- lapply(
  nn.fits, 
  nn.get.predictions, 
  data = norm.train %>% 
    select(-loan_status) %>% 
    as.matrix()
)
# Mittelwerte über alle Modelle
pred.exakt.train.nn <- rowMeans(do.call(cbind, nn.all.predictions.train)) 
pred.round.train.nn <- pred.exakt.train.nn %>% 
  round()

# Testdaten Vorhersagen (Matrix)
nn.all.predictions.test <- lapply(
  nn.fits,
  nn.get.predictions, 
  data = norm.test %>% 
    select(-loan_status) %>% 
    as.matrix()
)
# Mittelwerte über alle Modelle
pred.exakt.test.nn <- rowMeans(do.call(cbind, nn.all.predictions.test)) 
pred.round.test.nn <- pred.exakt.test.nn %>% 
  round()

# Analyse der Verlustfunktion

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