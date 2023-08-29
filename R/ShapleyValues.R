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