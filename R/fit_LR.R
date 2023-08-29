lr <- glm(loan_status ~ ., data=norm.train, family="binomial")

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

logit<-function(x) 1/(1+exp(-x))
pred.exakt.train.lr<-(
  sweep(norm.train[,-12], 2, lr$coefficients[2:12], "*") %>%
    rowSums()+lr$coefficients[1]
  ) %>%
  logit()
pred.exakt.test.lr<-(
  sweep(norm.test[,-12], 2, lr$coefficients[2:12], "*") %>%
    rowSums()+lr$coefficients[1]
  ) %>%
  logit()

pred.round.train.lr<-pred.exakt.train.lr %>% round()
pred.round.test.lr<-pred.exakt.test.lr %>% round()