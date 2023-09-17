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
results <- lapply(1:15, function(i) fit.nn())
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
