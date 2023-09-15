# Modellierung xgBoost
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

# Vorhersagen 
pred.exakt.train.xg<-predict(xgb_mod.s,xgtrain)
pred.round.train.xg<-pred.exakt.train.xg %>% 
  round()
pred.exakt.test.xg<-predict(xgb_mod.s,xgtest)
pred.round.test.xg<- pred.exakt.test.xg %>%
  round()
eval_log<-data.frame(xgb_mod.s$evaluation_log)

# Analyse des Trainingsprozesses
p1<-ggplot(eval_log, aes(x = iter)) +
  geom_line(aes(y = train_error, color = "Trainingsfehler"), size = 1) +
  geom_point(aes(y = train_error, color = "Trainingsfehler"), size = 3) +
  geom_line(aes(y = valid_error, color = "Validierungsfehler"), size = 1) +
  geom_point(aes(y = valid_error, color = "Validierungsfehler"), size = 3) +
  labs(title = "Trainings- und Validierungsfehler", x = "Iteration", y = "Fehler") +
  scale_color_manual(
    name = "Fehlerart",
    values = c("Trainingsfehler" = "#0d421f", "Validierungsfehler" = "orange"),
    labels = c("Trainingsfehler", "Validierungsfehler")
  ) +
  theme_minimal()
print(p1)
