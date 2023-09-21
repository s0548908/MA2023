# SHAP NN Import
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
# Trainingsdaten
shap.train.nn <- Reduce(`+`, lapply(shap_values_list_train, `[[`, 1)) /
  length(nn.fits)
shap.train.nn<-data.frame(shap.train.nn)
names(shap.train.nn)<-names(norm.train[-12])
shap.train.nn$BIAS<-mean(pred.exakt.train.nn)
shap.nn.sum.train<-rowSums(shap.train.nn)

# Testdaten
shap.test.nn <- Reduce(`+`, lapply(shap_values_list_test, `[[`, 1)) /
  length(nn.fits)
shap.test.nn<-data.frame(shap.test.nn)
names(shap.test.nn)<-names(norm.test[-12])
shap.test.nn$BIAS<-mean(pred.exakt.test.nn)
shap.nn.sum.test<-rowSums(shap.test.nn)