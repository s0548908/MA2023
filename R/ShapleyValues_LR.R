# Trainingsdaten
shap.train.lr<-norm.train[,1:11] %>%
  as.matrix() %*% diag(lr$coefficients[2:12]) -
  matrix(
    lr$coefficients[2:12] * colMeans(norm.train[, 1:11]),
    nrow=nrow(norm.train),
    ncol=11,
    byrow=TRUE
    )
shap.train.lr<-data.frame(shap.train.lr)
# Testdaten
shap.test.lr<-norm.test[,1:11] %>%
  as.matrix() %*% diag(lr$coefficients[2:12]) -
  matrix(
    lr$coefficients[2:12] * colMeans(norm.train[, 1:11]),
    nrow=nrow(norm.test),
    ncol=11,
    byrow=TRUE
  )
shap.test.lr<-data.frame(shap.test.lr)


names(shap.train.lr)<-names(norm.train[-12])
shap.train.lr$BIAS<-shap.train.lr$BIAS<-predict(lr) %>%
  mean() 
shap.lr.sum.train<-rowSums(shap.train.lr)


names(shap.test.lr)<-names(norm.test[-12])
shap.test.lr$BIAS<-predict(lr,newdata = norm.test) %>%
  mean() 
shap.lr.sum.test<-rowSums(shap.test.lr)
