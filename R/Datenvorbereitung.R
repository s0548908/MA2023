loan_data <- read.csv(
  "~/GitHub/MA2023/data/loan_approval_dataset.csv",
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

# Aufteilung in Trainings- und Testdaten ####
set.seed(123)
idx<-sample(nrow(loan_data),nrow(loan_data)*.8,F)
tbltrain<-loan_data[idx,]
tbltest<-loan_data[-idx,]

# Normierung der Trinings- und Testdaten ####
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