# Format für die Bezeichnung des Waterfall Plots
f <- function(x) format(x, big.mark = ".", decimal.mark =",", scientific = FALSE)

individualPlot<-function(shapData,id,referenzData){
  # Daten neu strukturieren
  data <- data.frame(
    variable = colnames(shapData[c(12,1:11)]), 
    value = as.numeric(shapData[id,c(12,1:11)])
  )
  data$variable <- factor(data$variable, levels = rev(data$variable))
  
  # Plot 1 Balkendiagram
  p1<-ggplot(data, aes(x = value, y = variable, fill = value > 0)) + 
    geom_col() +
    scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
    labs(
      title = "Individual SHAP Values \n USER 125 ",
      x = "Variable",
      y = "SHAP Value"
    ) +
    theme_minimal()+
    theme(
      axis.title.x = element_text(family = "LM Roman 10"),  
      axis.title.y = element_text(family = "LM Roman 10"),  
      axis.text.x = element_text(family = "LM Roman 10"),   
      axis.text.y = element_text(family = "LM Roman 10"),   
      strip.text = element_text(family = "LM Roman 10"),    
      plot.title = element_text(family = "LM Roman 10"),
      legend.text = element_text(family = "LM Roman 10"),   
      legend.title = element_text(family = "LM Roman 10"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )+
  coord_cartesian(ylim = c(-1.5, NA))
  
  # Kummulierte Daten berechnen
  features <- colnames(shapData)[c(12, 1:11)]
  cumulative_values <- cumsum(shapData[id, c(12, 1:11)] %>% as.vector())
  cumulative_values<-1/(1+exp(-cumulative_values))
  
  
  # Kreise vorbereiten
  arrow_x <- cumulative_values
  arrow_y <- 12:1
  base_x <- c(.5, cumulative_values) %>% unlist()
  base_y <- 12:0
  
  # Achsen beschriftung vorbereiten
  tbl <- tibble(
    a = c(min(cumulative_values), max(cumulative_values) , cumulative_values[12]),
    b = c(min(cumulative_values) %>% round(1), 
          max(cumulative_values) %>% round(1),
          paste("Endwert", round(cumulative_values[12], 2))
    )
  ) %>% 
    arrange(a)
  # Waterfall Plot
  p2<-ggplot() +
    annotate(
      "segment",
      x = base_x[-length(base_x)],
      xend = base_x[-1],
      y = base_y[-13],
      yend = base_y[-13],
      arrow = arrow(type = "closed", length = unit(0.05, "inches")),
      color = ifelse(diff(c(base_x)) > 0, "green", "red"),
      size = .2  # Hier die Linienstärke anpassen
    )+
    geom_text(
      aes(
        x = base_x[-length(base_x)], 
        y = base_y[-13]+.2, 
        label =
          ifelse(
            diff(c(base_x))>0,
            paste("+", diff(c(base_x)) %>% round(3) %>% as.character()),
            diff(c(base_x)) %>% round(3) %>% as.character()
          )
      ),
      size=3.5,
      color="gray"
    )+
    scale_x_continuous(
      name = "Prognosewert", 
      breaks = tbl$a, 
      labels = tbl$b
    ) +
    scale_y_continuous(
      name = "", 
      breaks = 12:0, 
      labels = paste(
        c(names(base_x)[2:13],"Endwert"),
        "\n=",
        c(cumulative_values[1]
          %>%round(3),
          referenzData[id,2:12] 
          %>%f(),
          cumulative_values[12] %>%
            round(3)
        )
      )
    ) +
    labs(title = "Kummulierte \n Shapley Values") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank()
    )
  
  grid.arrange(p1, p2, ncol = 2)
}