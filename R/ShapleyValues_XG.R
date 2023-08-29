# SHAP XG 
shap.train.xg <- predict(xgb_mod.s, xgtrain, predcontrib = TRUE) %>%
  data.frame()
shap.test.xg <- predict(xgb_mod.s, xgtest, predcontrib = TRUE) %>%
  data.frame()
shap.xg.sum.train<-rowSums(shap.train.xg)
shap.xg.sum.test<-rowSums(shap.test.xg)
