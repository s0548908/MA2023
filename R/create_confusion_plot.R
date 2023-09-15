confusion_plot <- function(pred_train, pred_test, plot_title) {
  # Konfusionsdaten berechnen
  df_train <- caret::confusionMatrix(
    pred_train %>%
      factor(),
    norm.train$loan_status %>%
      factor()
  )$table %>%
    as.data.frame()
  df_test <- caret::confusionMatrix(
    pred_test %>%
      factor(),
    norm.test$loan_status %>%
      factor()
  )$table %>%
    as.data.frame()
  # Daten zusammenfügen und Labels hinzufügen
  df <- rbind(
    transform(
      df_train, 
      dataset = "Trainingsdaten", 
      label = paste(c("TN", "FN", "FP", "TP"), Freq)
    ),
    transform(
      df_test, 
      dataset = "Testdaten", 
      label = paste(c("TN", "FN", "FP", "TP"), Freq)
    )
  )
  
  # Levels und Labels setzen
  levels(df$Prediction) <- c("Abgelehnt", "Genehmig")
  levels(df$Reference) <- c("Abgelehnt", "Genehmig")
  
  # Plot erstellen
  ggplot(
    df, 
    aes(x = Reference, y = Freq, fill = Prediction)
  ) +
    geom_bar(
      stat = "identity", 
      position = "stack"
    ) +
    geom_text(
      aes(label = label), 
      position = position_stack(vjust = 0.5),
      size = 10
    ) +
    facet_wrap(
      ~dataset, 
      scales = "free_y"
    ) +
    labs(
      x = "Vorhersage",
      y = "Anzahl",
      fill = "Status",
      title = plot_title
    ) +
    theme_minimal()+
    theme(
      text = element_text(family = "Times New Roman",size = 20),
      axis.title = element_text(family = "Times New Roman",size = 20),
      axis.text = element_text(family = "Times New Roman",size = 20),
      legend.title = element_text(family = "Times New Roman",size = 20),
      legend.text = element_text(family = "Times New Roman",size = 20),
      plot.title = element_text(family = "Times New Roman",size = 20)
    )
}