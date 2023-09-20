## Überprüfe BIAS Wert für ein Modell:
tmp.conv<-Converter$new(
  nn.fits[[1]],
  input_dim = c(11)
)
tmp.shap<-DeepLift$new(
  tmp.conv,
  as.matrix(norm.train %>% select(-loan_status)),
  x_ref = colMeans(norm.train[-12]) %>% as.matrix() %>% t(),
  rule_name = "reveal_cancel"
)
tmp.shap<-get_result(tmp.shap)[,,1]
p<- predict(nn.fits[[1]],norm.train %>% select(-loan_status) %>% as.matrix())[,1]

equation <- function(i,x, shap, prediction) {
  1 / (1 + exp(-(rowSums(shap)[i] + x))) - prediction[i]
}

# Funktioniert in etwa für die ersten 5 Instanzen
# Evtl interne Rundungsfehler 
#entspricht jedoch nicht dem Mittelwert der Prognosen:
lapply(1:5,function(i){
  uniroot(
    equation,
    interval = c(-100, 100),
    shap = tmp.shap,
    prediction = p,
    i=i,
    maxiter=100
  )$root  
}) %>%
  unlist() 

# finale BIAS für das erste Modell:
lapply(1:nrow(norm.train),function(i){
  uniroot(
    equation,
    interval = c(-100, 100),
    shap = tmp.shap,
    prediction = p,
    i=i,
    maxiter=100
  )$root  
}) %>%
  unlist() %>% 
  mean()


# Verallgemeinern auf alle MOdelle:
converters <- lapply(nn.fits, function(model) {
  Converter$new(
    model,
    input_dim = c(11)
  )
})

# Für die Trainingsdaten:
shap_values_list_train <- lapply(converters, function(converter) {
  DeepLift$new(
    converter,
    as.matrix(norm.train %>% select(-loan_status)),
    x_ref = colMeans(norm.train[-12]) %>% as.matrix() %>% t(),
    rule_name = "reveal_cancel"
  )
})

shap_values_list_test <- lapply(converters, function(converter) {
  DeepLift$new(
    converter,
    as.matrix(norm.test %>% select(-loan_status)),
    x_ref = colMeans(norm.train[-12]) %>% as.matrix() %>% t(),
    rule_name = "reveal_cancel"
  )
})

shap.train.nn <- Reduce(
  `+`, 
  lapply(shap_values_list_train, function(mat) get_result(mat)[,,1])
)/
  length(nn.fits)
shap.train.nn<-data.frame(shap.train.nn)
names(shap.train.nn)<-names(norm.train[-12])

shap.test.nn <- Reduce(
  `+`, 
  lapply(shap_values_list_test, function(mat) get_result(mat)[,,1])
)/
  length(nn.fits)
shap.test.nn<-data.frame(shap.test.nn)
names(shap.test.nn)<-names(norm.test[-12])

BIAS<-lapply(1:nrow(norm.train),function(i){
  uniroot(
    equation,
    interval = c(-100, 100),
    shap = shap.train.nn,
    prediction = pred.exakt.train.nn,
    i=i
  )$root  
}) %>%
  unlist() %>%
  mean()
shap.train.nn$BIAS<-BIAS
l<-rowSums(shap.train.nn)

shap.test.nn$BIAS<-mean(pred.exakt.train.nn)
shap.nn.sum.test<-rowSums(shap.test.nn)

