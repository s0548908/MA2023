SummenShapleyPlot <- function(data, title, x_label, legend = F) {
  plot <- data %>%
    mutate(
      pred = pred + rnorm(length(data$pred), 0, 0.015)
    ) %>%
    ggplot(aes(x = pred, y = shap, color = Richtig)) +
    geom_point() +
    scale_color_manual(values = c("red", "black")) +
    labs(
      title = title,
      x = x_label,
      y = paste("SHAP", x_label, "Sum")
    ) +
    scale_x_continuous(breaks = c(0, 1), labels = c("Abgelehnt", "Genehmigt")) +
    theme_minimal()
  
  if (!legend) {
    plot <- plot + theme(legend.position = "none")
  }
  
  return(plot)
}