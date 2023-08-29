featureImportance.Plot<-function(data,titel){
  # Umwandeln in Long Format
  shap.train.long<-shap.prep(
    shap_contrib = data[-12], 
    X_train = norm.train[-12]
    )
  shap.train.long$y_values <- rnorm(nrow(shap.train.long), 0, 0.05)
  
  # Hinzufügen von y_values und Farben
  shap.train.long <- shap.train.long %>%
    group_by(variable) %>%
    mutate(
      correlation = cor(value, rfvalue),
      color = ifelse(correlation > 0, -value, value)
    ) %>%
    ungroup()
  
  # Erstellen von y-Achsen-Labels
  txt <- shap.train.long %>%
    group_by(variable) %>%
    summarise(
      mean = sprintf(
        "%.3f", 
        value %>% 
          abs %>% 
          mean()
        )
      )
  # Plot erstellen
  ggplot(
    shap.train.long, 
    aes(x = value, y = y_values + as.numeric(factor(variable)), color = color)) +
    geom_point(size = 1) +
    scale_y_continuous(breaks = 1:11, labels = paste(txt$variable,txt$mean)) +
    scale_color_gradient(
      low = "orange", 
      high = "#0d421f", 
      name = "Merkmal \n(hoch =grün)") +
    labs(
      title = titel,
      x = "Shapley Values",
      y = "",
      color = "Merkmal") +
    theme_minimal() 
}