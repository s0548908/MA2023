weights.list <- lapply(nn.fits, function(model) {
  model$get_weights()
})
# Kanten definieren
edges<-tibble(
  from=c(rep(1:43, each = 16),44:59),
  to=c(
    rep(12:27, times=11),
    rep(28:43, times = 16),
    rep(44:59, times = 16), 
    rep(60, times = 16)
  ),
  label=weights.list[[1]][c(1,3,5,7)] %>% 
    unlist() %>% 
    round(4) %>% 
    as.character()
)
edges$font.color <- "orange"
edges$color <- "#8105f5"
edges$color[edges$from %in% 1:11] <- "orange"
edges$color[177:433]<-"#b11111"
edges$color[434:690]<-"green"
weights<-weights.list[[1]][c(2,4,6,8)] %>% 
  unlist()
# Koten definieren
nodes <- data.frame(
  id = 1:60, 
  label = paste(
    1:60,"Neuron"," 
    Bias:",
    c(rep(NA,11),round(weights,2))
  )
)
nodes$group<-c(
  rep("Eingangsschicht",11),
  rep("Layer 1",16),
  rep("Layer 2",16),
  rep("Layer 3",16),
  "Ausgangsschicht"
)

nodes$shape<-c(
  rep("box",11),
  rep("dot",16),
  rep("dot",16),
  rep("dot",16),
  "star")
nodes$color<-c(
  rep("orange",11),
  rep("#b11111",16),
  rep("green",16),
  rep("#8105f5",16),
  "orange")
#Grafik erstellen
p<-visNetwork(nodes, edges,width = "100%", height = "800px") %>%
  visGroups(groupname = "Ausgangsschicht", color = "orange",shape="star") %>%
  visGroups(groupname = "Layer 3", color = "#8105f5",shape="dot") %>%
  visGroups(groupname = "Layer 2", color = "green",shape="dot") %>%
  visGroups(groupname = "Layer 1", color = "#b11111",shape="dot") %>%
  visGroups(groupname = "Eingangsschicht", color = "orange",shape="box") %>%
  visLegend() %>%
  visLayout(
    randomSeed = 3,
    improvedLayout = TRUE
  ) %>% 
  visEdges(arrows = "to") %>%
  visNodes(font = list(color = "black")) %>%
  visOptions(
    highlightNearest = list(
      enabled = T, 
      degree = 0,
      hover=T), 
    nodesIdSelection = F) %>%
  visPhysics(
    stabilization = list(
      enabled = TRUE, 
      iterations = 100
    ),
    solver = "forceAtlas2Based",
    forceAtlas2Based = list(
      gravityConstant = 0,
      springLength = 50,
      springConstant = 0.01)
  )

print(p)