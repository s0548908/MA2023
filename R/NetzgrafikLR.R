edges<-tibble(
  from=c(1:11,12:23),
  to = c(12:22,rep(24,12)),
  color="#8105f5",
  font.color="orange",
  label  = c(rep("",11),rep("+",12))
)
nodes<-tibble(
  id=1:24,
  levels=c(
    rep(1,11),
    rep(2,12),
    3),
  label = c(
    colnames(loan_data[2:12]),
    paste(
      "beta",
      1:11,
      "\n",
      colnames(loan_data[2:12]), 
      "\n",
      lr$coefficients[c(2:12)] %>% 
        round(digits = 4)
    ),
    paste(
      "Intercept\n",      
      lr$coefficients[c(1)] 
      %>% round(digits = 4)
    ),
    "Ausgang"
  ),
  shape=c(
    rep("box",11),
    rep("dot",12),
    "star"
  ),
  size =c(
    rep(25,11),
    lr$coefficients[c(2:12,1)] 
    %>% abs(),
    25 ),
  color=c(
    rep("orange",11),
    rep("#922ef0",12),
    "orange"
  )
)
p<-visNetwork(nodes, edges) %>%
  visEdges(arrows = "to") %>%
  visNodes(
    font = list(
      color = "green"
    ), 
    size = nodes$size
  ) %>%
  visOptions(
    highlightNearest = list(
      enabled = T, 
      degree = 1, 
      hover = T
    ), 
    nodesIdSelection = TRUE) %>%
  visLayout(
    randomSeed = 42, 
    improvedLayout = TRUE, 
    hierarchical = list(
      enabled = TRUE, 
      direction = 'DU', 
      sortMethod = 'directed')
  ) 

print(p)