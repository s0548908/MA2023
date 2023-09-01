# Kanten definieren
edges<-tibble(
  from=1:24,
  to = c(12:22,rep(24,12),25),
  color="#8105f5",
  font.color="#8105f5",
  label  = c(rep("*",11),rep("",12),"sigmoid")
)
# Knoten definieren
nodes<-tibble(
  id=1:25,
  levels=c(rep(1,11),rep(2,12),3,4),
  label = c(
    paste(
      "Eingang \n",
      names(norm.train[1:11])
    ),
    paste(
      "beta",1:11,"\n",
      names(norm.train[1:11]),"\n",
      lr$coefficients[c(2:12)] %>% 
        round(digits = 4)
    ),
    paste(
      "Intercept\n",      
      lr$coefficients[c(1)] %>% 
        round(digits = 4)
    ),
    "∑",
    "Ziel/Ausgang"
  ),
  shape=c(
    rep("box",11),
    rep("dot",12),
    "box",
    "star"),
  size =c(
    rep(25,11),
    lr$coefficients[c(2:12,1)] %>% 
      abs(),
    rep(25,2) 
  ),
  color=c(
    rep("orange",11),
    rep("#922ef0",13),
    "orange"
  )
)
# Plot erstellen
p <-visNetwork(
  nodes, 
  edges,width = "100%", 
  height = "1100px"
)

print(p)
