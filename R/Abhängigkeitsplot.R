Abhängigkeitsplot<-function(data,pred,Merkmal,titel){
  # Kurve erzeugen
  loess_fit <- loess(data[,Merkmal] ~ norm.train[,Merkmal])
  predicted_values <- predict(
    loess_fit, 
    newdata = data.frame(x = sort(norm.train[,Merkmal]))
  )
  # Korrelationstest
  cor.test.data<-cor.test(
    norm.train[,Merkmal],
    data[,Merkmal]
  )
  # Korreltationstext für Grafik
  cor_text <- paste0(
    "R = ", round(cor.test.data$estimate, 2),
    ", p < ", ifelse(
      round(cor.test.data$p.value, 6) == 0, 
      "2.2e-16", 
      round(cor.test.data$p.value, 4)
    )
  )
  # Daten extrahieren
  filtered_data <- tibble(
    rfvalue = norm.train[,Merkmal], 
    value = data[,Merkmal], 
    richtig = pred == norm.train$loan_status, 
    loan_status = as.factor(norm.train$loan_status)
  )
  filtered_data$richtig <- factor(
    filtered_data$richtig, 
    levels = c(TRUE, FALSE), 
    labels = c("Richtige Vorhersage", "Falsche Vorhersage")
  )
  # Plot 1 Scatter 
  scatter <- ggplot(
    filtered_data, 
    aes(
      x = rfvalue, 
      y = value, 
      color = loan_status, 
      shape = richtig)
  ) +
    geom_point(aes(size = richtig)) +  
    geom_smooth(aes(group = 1), method = "loess", se = FALSE, color = "#0d421f")+
    scale_color_manual(values = c("red", "black")) +
    scale_shape_manual(values = c(16, 17)) +   
    scale_size_manual(
      values = c("Richtige Vorhersage" = 1, "Falsche Vorhersage" = 5)) +  
    labs(
      title=paste(cor_text,titel),
      x = "Referenz Wert",
      y = "Shapley Value",
      color = "Vorhersage"
    ) +
    theme_minimal()
  # Plot 2 Hist
  hist_x <- ggplot(filtered_data, aes(x = rfvalue)) +
    geom_histogram(
      aes(y = ..density..), 
      fill = "orange", 
      color = "#0d421f", 
      bins = 30) +
    theme_minimal()
  # Plot 3 Hist
  hist_y <- ggplot(filtered_data, aes(x = value)) +
    geom_histogram(
      aes(y = ..density..), 
      fill = "orange", 
      color = "#0d421f", 
      bins = 30) +
    coord_flip() +
    theme_minimal()
  
  grid.arrange(
    hist_x, 
    NULL, 
    scatter, 
    hist_y, 
    ncol = 2, 
    nrow = 2, 
    widths = c(4, 1), 
    heights = c(1, 4)
  )
}