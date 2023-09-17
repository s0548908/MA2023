#xgb_mod.s<-xgb.load("xgbtmp2.model")
f <- function(x) format(x, big.mark = ".", decimal.mark =",", scientific = FALSE)
# Start f ####
function(input, output, session) {

  
  #  **Datensatz** ####
  output$kab.feat<-renderUI({
    features <- data.frame(
      Merkmal = c(
        "UserID (loan id)", 
        "Abhängigen Personen  (no of dependents)", 
        "Bildungsstand  (education)", 
        "Selbstständigkeit  (self employed)", 
        "Jahreseinkommen  (income annum)", 
        "Kredithöhe (loan amount)", 
        "Kreditlaufzeit  (loan term)", 
        "Kreditscore  (cibil score)", 
        "Wohnimmobilien (residential assets value)", 
        "Gewerbliches Vermögen  (commercial assets value)", 
        "Luxusgüter  (luxury assets value)", 
        "Bankvermögen  (bank asset value)", 
        "Kreditstatus  (loan status)"
        ),
      Beschreibung = c(
        "Eindeutige Identifikationsnummer der UserID",
         "Anzahl der vom Antragsteller abhängigen Personen.",
         "Bildungsstand Antragsteller (Absolvent / Nicht-Absolvent).",
         "Selbstständigkeit des Antragstellers (Ja/Nein).",
         "Jährliches Einkommen des Antragstellers.",
         "Kreditbetrag.",
         "Laufzeit des Kredits in Jahren.",
         "Kredit-Score des Antragstellers.",
         "Wert der Wohnimmobilien des Antragstellers.",
         "Wert der gewerblichen Immobilien des Antragstellers.",
         "Wert der Luxusgüter des Antragstellers.",
         "Wert der Bankvermögen des Antragstellers.",
         "Status des Kreditantrags (Genehmigt/Abgelehnt)."
        )
    )
    kable(
      features, 
      "html",  
      longtable = TRUE, 
      caption = "Merkmale des Datensatzes") %>%
      kable_styling() %>%
      HTML()
  })
  # **Trainings - und Testdaten** ####
  ## Trainingsdaten ####
  output$train.data.prozent  <- renderValueBox({
    valueBox(
      "80 %",
      paste("Trainingsdaten (",nrow(tbltrain),")"),
      color = "navy"
    )
  })
  output$train.data.1  <- renderValueBox({
    valueBox(
      paste(
        round(
          sum(tbltrain$loan_status==1)/nrow(tbltrain)*100 ,
          2
        ),
        "%"
      ),
      paste(
        "Kredit genehmigt (",
        sum(tbltrain$loan_status==1),
        ")"
      ),
      color = "navy"
    )
  })
  output$train.data.0  <- renderValueBox({
    valueBox(
      paste(
        round(
          sum(tbltrain$loan_status==0)/nrow(tbltrain)*100 ,
          2
        ),
        "%"
      ),
      paste("Kredit abgelehnt (",
            sum(tbltrain$loan_status==0),
            ")"
      ),
      color = "navy"
    )
  })
  output$train.data.v1  <- renderValueBox({
    valueBox(
      paste(
       min(tbltrain$no_of_dependents),
        "|",
        median(tbltrain$no_of_dependents),
        "|",
        max(tbltrain$no_of_dependents)
      ),
      HTML(
        paste(
          "Kennzahlen: Min | Median | Max <br>
          Feature: no_of_dependents <br> 
          DE: Anzahl der Familienangehörigen" 
        )
      ),
      color = "navy"
    )
  })
  output$train.data.v2  <- renderValueBox({
    valueBox(
      paste(
        median(tbltrain$income_annum) %>%
          f()
      ),
      HTML(
        paste(
          "Kennzahlen: Median <br>
          Feature: income_annum <br> 
          DE: Jahreseinkommen " 
        )
      ),
      color = "navy"
    )
  })
  output$train.data.v3  <- renderValueBox({
    valueBox(
      paste(
        median(tbltrain$loan_amount) %>%
          f()
      ),
      HTML(
        paste(
          "Kennzahlen: Median <br>
          Feature: loan_amount <br> 
          DE: Kreditbetrag " 
        )
      ),
      color = "navy"
    )
  })
  output$train.data.v4  <- renderValueBox({
    valueBox(
      paste(
        min(tbltrain$loan_term),
        "|",
        median(tbltrain$loan_term),
        "|",
        max(tbltrain$loan_term)
      ),
      HTML(
        paste(
          "Kennzahlen: Min | Median | Max <br>
          Feature: loan_term <br> 
          DE: Kredit Laufzeit (Jahre)" 
        )
      ),
      color = "navy"
    )
  })
  output$train.data.v5  <- renderValueBox({
    valueBox(
      paste(
        min(tbltrain$cibil_score),
        "|",
        median(tbltrain$cibil_score),
        "|",
        max(tbltrain$cibil_score)
      ),
      HTML(
        paste(
          "Kennzahlen: Min | Median | Max <br>
          Feature: cibil_score <br> 
          DE: Kredit-Score " 
        )
      ),
      color = "navy"
    )
  })
  output$train.data.v6  <- renderValueBox({
    valueBox(
      paste(
        median(tbltrain$residential_assets_value) %>% f()
      ),
      HTML(
        paste(
          "Kennzahlen:  Median <br>
          Feature: residential_assets_value <br> 
          DE: Wert der Wohnimmobilien " 
        )
      ),
      color = "navy"
    )
  })
  output$train.data.v7  <- renderValueBox({
    valueBox(
      paste(
        median(tbltrain$commercial_assets_value) %>% f()
      ),
      HTML(
        paste(
          "Kennzahlen:  Median <br>
          Feature: commercial_assets_value <br> 
          DE: Wert der Gewerbeimmobilien" 
        )
      ),
      color = "navy"
    )
  })
  output$train.data.v8  <- renderValueBox({
    valueBox(
      paste(
        median(tbltrain$luxury_assets_value) %>% f()
      ),
      HTML(
        paste(
          "Kennzahlen:  Median <br>
          Feature: luxury_assets_value <br> 
          DE: Wert von Luxusgütern" 
        )
      ),
      color = "navy"
    )
  })
  output$train.data.v9  <- renderValueBox({
    valueBox(
      paste(
        median(tbltrain$bank_asset_value) %>% f()
      ),
      HTML(
        paste(
          "Kennzahlen:  Median <br>
          Feature: bank_asset_value <br> 
          DE: Wert der Bankvermögenswerte" 
        )
      ),
      color = "navy"
    )
  })
  output$train.data.v10  <- renderValueBox({
    valueBox(
      paste(
        "J:",
       sum(tbltrain$education_Graduate==1),
        "N:",
       sum(tbltrain$education_Graduate==0)
      ),
      HTML(
        paste(
          "Kennzahlen: Anzahl<br>
          Feature: education_Graduate <br> 
          DE: Hochschulabschluss" 
        )
      ),
      color = "navy"
    )
  })
  output$train.data.v11  <- renderValueBox({
    valueBox(
      paste(
        "J:",
        sum(tbltrain$self_employed_No==1),
        "N:",
        sum(tbltrain$education_Graduate==0)
      ),
      HTML(
        paste(
          "Kennzahlen: Anzahl<br>
          Feature: self_employed_No <br> 
          DE: Nicht selbstständig" 
        )
      ),
      color = "navy"
    )
  })
  
  ## testdaten ####
  output$test.data.prozent  <- renderValueBox({
    valueBox(
      "20 %",
      paste("Testdaten (",nrow(tbltest),")"),
      color = "navy"
    )
  })
  output$test.data.1  <- renderValueBox({
    valueBox(
      paste(
        round(
          sum(tbltest$loan_status==1)/nrow(tbltest)*100 ,
          2
        ),
        "%"
      ),
      paste("Kredit genehmigt (",
            sum(tbltest$loan_status==1),
            ")"
      ),
      color = "navy"
    )
  })
  output$test.data.0  <- renderValueBox({
    valueBox(
      paste(
        round(
          sum(tbltest$loan_status==0)/nrow(tbltest)*100 ,
          2
        ),
        "%"
      ),
      paste(
        "Kredit abgelehnt (",
        sum(tbltest$loan_status==0),
        ")"
      ),
      color = "navy"
    )
  })
  output$test.data.v1  <- renderValueBox({
    valueBox(
      paste(
        min(tbltest$no_of_dependents),
        "|",
        median(tbltest$no_of_dependents),
        "|",
        max(tbltest$no_of_dependents)
      ),
      HTML(
        paste(
          "Kennzahlen: Min | Median | Max <br>
          Feature: no_of_dependents <br> 
          DE: Anzahl der Familienangehörigen" 
        )
      ),
      color = "navy"
    )
  })
  output$test.data.v2  <- renderValueBox({
    valueBox(
      paste(
        median(tbltest$income_annum) %>%
          f()
      ),
      HTML(
        paste(
          "Kennzahlen: Median <br>
          Feature: income_annum <br> 
          DE: Jahreseinkommen " 
        )
      ),
      color = "navy"
    )
  })
  output$test.data.v3  <- renderValueBox({
    valueBox(
      paste(
        median(tbltest$loan_amount) %>%
          f()
      ),
      HTML(
        paste(
          "Kennzahlen: Median <br>
          Feature: loan_amount <br> 
          DE: Kreditbetrag " 
        )
      ),
      color = "navy"
    )
  })
  output$test.data.v4  <- renderValueBox({
    valueBox(
      paste(
        min(tbltest$loan_term),
        "|",
        median(tbltest$loan_term),
        "|",
        max(tbltest$loan_term)
      ),
      HTML(
        paste(
          "Kennzahlen: Min | Median | Max <br>
          Feature: loan_term <br> 
          DE: Kredit Laufzeit (Jahre)" 
        )
      ),
      color = "navy"
    )
  })
  output$test.data.v5  <- renderValueBox({
    valueBox(
      paste(
        min(tbltest$cibil_score),
        "|",
        median(tbltest$cibil_score),
        "|",
        max(tbltest$cibil_score)
      ),
      HTML(
        paste(
          "Kennzahlen: Min | Median | Max <br>
          Feature: cibil_score <br> 
          DE: Kredit-Score " 
        )
      ),
      color = "navy"
    )
  })
  output$test.data.v6  <- renderValueBox({
    valueBox(
      paste(
        median(tbltest$residential_assets_value) %>% f()
      ),
      HTML(
        paste(
          "Kennzahlen:  Median <br>
          Feature: residential_assets_value <br> 
          DE: Wert der Wohnimmobilien " 
        )
      ),
      color = "navy"
    )
  })
  output$test.data.v7  <- renderValueBox({
    valueBox(
      paste(
        median(tbltest$commercial_assets_value) %>% f()
      ),
      HTML(
        paste(
          "Kennzahlen:  Median <br>
          Feature: commercial_assets_value <br> 
          DE: Wert der Gewerbeimmobilien" 
        )
      ),
      color = "navy"
    )
  })
  output$test.data.v8  <- renderValueBox({
    valueBox(
      paste(
        median(tbltest$luxury_assets_value) %>% f()
      ),
      HTML(
        paste(
          "Kennzahlen:  Median <br>
          Feature: luxury_assets_value <br> 
          DE: Wert von Luxusgütern" 
        )
      ),
      color = "navy"
    )
  })
  output$test.data.v9  <- renderValueBox({
    valueBox(
      paste(
        median(tbltest$bank_asset_value) %>% f()
      ),
      HTML(
        paste(
          "Kennzahlen:  Median <br>
          Feature: bank_asset_value <br> 
          DE: Wert der Bankvermögenswerte" 
        )
      ),
      color = "navy"
    )
  })
  output$test.data.v10  <- renderValueBox({
    valueBox(
      paste(
        "J:",
        sum(tbltest$education_Graduate==1),
        "N:",
        sum(tbltest$education_Graduate==0)
      ),
      HTML(
        paste(
          "Kennzahlen: Anzahl<br>
          Feature: education_Graduate <br> 
          DE: Hochschulabschluss" 
        )
      ),
      color = "navy"
    )
  })
  output$test.data.v11  <- renderValueBox({
    valueBox(
      paste(
        "J:",
        sum(tbltest$self_employed_No==1),
        "N:",
        sum(tbltest$education_Graduate==0)
      ),
      HTML(
        paste(
          "Kennzahlen: Anzahl<br>
          Feature: self_employed_No <br> 
          DE: Nicht selbstständig" 
        )
      ),
      color = "navy"
    )
  })

  # **Modelle** ####
  ## Logistic Regression ####
  output$tmp.lr<-renderPrint({
    summary(lr)
  })
  output$lr.conf<-renderPlotly({
    confusion_plot(
      pred_train = pred.round.train.lr,
      pred_test = pred.round.test.lr,
      plot_title = "Konfusionsdaten für Trainings- und Testdaten - Logistische Regression"
    )
  })
 
  output$network.lr <- renderVisNetwork({
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
    visNetwork(nodes, edges) %>%
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
  })
  ## Neuronales Netz  ####
  observeEvent (input$nn.fits, {
    updateSliderInput(
      session, 
      "nn.fits2", 
      min = 1, 
      max = 15, 
      value = as.numeric(input$nn.fits), 
      step = 1
      )
  })
  observeEvent (input$nn.fits2, {
    updateSliderInput(
      session, 
      "nn.fits", 
      min = 1, 
      max = 15, 
      value = as.numeric(input$nn.fits2), 
      step = 1
      )
  })
  output$nn.fits.metrics<-renderPlotly({
    history_data <- metrics.list[[as.numeric(input$nn.fits)]]$metrics
    epochs <- 1:75
    # Erstelle das Plotly-Diagramm
    plot_ly(
      x = ~epochs, 
      y = ~history_data$loss, 
      type = 'scatter', 
      mode = 'lines+markers', 
      name = 'Training loss',
      line = list(color = 'green'),
      marker = list(color = 'green')) %>%
      add_trace(
        y = ~history_data$val_loss, 
        name = 'Validation loss', 
        mode = 'lines+markers',
        line = list(color = 'orange'),
        marker = list(color = 'orange')) %>%
      layout(
        title = "Training and validation - loss ",
        yaxis = list(
          title = "Loss",
          tickfont = list(color = "#ffa500"), 
          titlefont = list(color = "#ffa500")
        ),
        xaxis = list(
          title = "Epochs",
          tickfont = list(color = "#ffa500"), 
          titlefont = list(color = "#ffa500")
        ),
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        plot_bgcolor = "rgba(0, 0, 0, 0)" 
      )
  })
  
  output$nn.fits.metrics2<-renderPlotly({
    history_data <- metrics.list[[as.numeric(input$nn.fits2)]]$metrics
    epochs <- 1:75
    plot_ly(
      x = ~epochs, 
      y = ~history_data$accuracy, 
      type = 'scatter', 
      mode = 'lines+markers', 
      name = 'Training accuracy',
      line = list(color = 'green'),
      marker = list(color = 'green')) %>%
      add_trace(
        y = ~history_data$val_accuracy, 
        name = 'Validation loss', 
        mode = 'lines+markers',
        line = list(color = 'orange'),
        marker = list(color = 'orange')) %>%
      layout(
        title = "Training and validation accuracy ",
        yaxis = list(
          title = "Loss",
          tickfont = list(color = "#ffa500"), 
          titlefont = list(color = "#ffa500")
        ),
        xaxis = list(
          title = "Epochs",
          tickfont = list(color = "#ffa500"), 
          titlefont = list(color = "#ffa500")
        ),
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        plot_bgcolor = "rgba(0, 0, 0, 0)" 
      )
  })
  
  output$nn.conf<-renderPlotly({
    confusion_plot(
      pred.round.train.nn,
      pred.round.test.nn,
      "Konfusionsdaten für Trainings- und Testdaten - Neuronales Netz"
    )
  })
  
  output$nn.heat<-renderPlotly({
    n<-input$nn.fits4
    if(n==1) m<-1
    if(n==2) m<-3
    if(n==3) m<-5
    weights <- weights.list[[input$nn.fits3]][[m]]
    weights_df <- as.matrix(weights)
    
    # Erstellen Sie eine Heatmap
    plot_ly(
      z = weights_df,
      colors = colorRamp(c("orange", "#0d421f")),
      type = "heatmap"
    ) %>%
      layout(
        title = paste(
          "Heatmap Gewichte der Neuronen | quad. Summe aller Neuronen einer Schicht: (",
          sum(weights_df^2) 
          %>% round(digits = 2),
          ")"
          ),
        xaxis = list(
          tickfont = list(color = "#ffa500"), 
          titlefont = list(color = "#ffa500"),
          gridcolor = "rgba(0,0,0,0)"),
        yaxis = list(
          tickfont = list(color = "#ffa500"), 
          titlefont = list(color = "#ffa500"),
          gridcolor = "rgba(0,0,0,0)"),
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        plot_bgcolor = "rgba(0, 0, 0, 0)"
      )
  })
  output$nndata<-renderDT({
    edges<-tibble(
      from=c(
        rep(1:43, each = 16),
        44:59
        ),
      to=c(
        rep(12:27, times = 11),
        rep(28:43, times = 16),
        rep(44:59, times = 16), 
        rep(60, times = 16)
      ),
      label=weights.list[[input$nn.fits5]][c(1,3,5,7)] %>% 
        unlist() %>% 
        round(4) %>% 
        as.character()
    )
    datatable(edges,rownames = F)
  })
  output$nndata2<-renderDT({
    get.weights.from.layer <- function(layer) {
      weights.list[[input$nn.fits5]][[layer]]
    }
    bias <- lapply(c(2,4,6,8), get.weights.from.layer) %>%
      unlist()
    nodes <- data.frame(
      label = paste0(12:60,". Neuron"),
      Bias = c(round(bias,3))
    )
    datatable(nodes,rownames = F)
  })
  output$network <- renderVisNetwork({
    edges<-tibble(
      from=c(
        rep(1:43, each = 16),
        44:59
        ),
      to=c(
        rep(12:27, times=11),
        rep(28:43, times = 16),
        rep(44:59, times = 16), 
        rep(60, times = 16)
      ),
      label=weights.list[[input$nn.fits5]][c(1,3,5,7)] %>% 
        unlist() %>% 
        round(4) %>% 
        as.character()
    )
    edges$font.color <- "orange"
    edges$color <- "#8105f5"
    edges$color[edges$from %in% 1:11] <- "orange"
    edges$color[177:433]<-"#b11111"
    edges$color[434:690]<-"green"
    get.weights.from.layer <- function(layer) {
      weights.list[[input$nn.fits5]][[layer]]
    }
    weights <- lapply(c(2,4,6,8), get.weights.from.layer) %>%
      unlist()
    nodes <- data.frame(
      id = 1:60, 
      label = paste(
        1:60,
        "Neuron",
        " Bias:",
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
      "star"
      )
    nodes$color<-c(
      rep("orange",11),
      rep("#922ef0",16),
      rep("#a95df0",16),
      rep("#8105f5",16),
      "orange"
      )
    
    visNetwork(nodes, edges,width = "100%", height = "800px") %>%
      visGroups(groupname = "Ausgangsschicht", color = "orange",shape="star") %>%
      visGroups(groupname = "Layer 3", color = "#8105f5",shape="dot") %>%
      visGroups(groupname = "Layer 2", color = "green",shape="dot") %>%
      visGroups(groupname = "Layer 1", color = "#b11111",shape="dot") %>%
      visGroups(groupname = "Eingangsschicht", color = "orange",shape="box") %>%
      visLegend() %>%
      visLayout(
        randomSeed = 3,
        improvedLayout = F
      ) %>% 
      visEdges(arrows = "to") %>%
      visNodes(font = list(color = "black")) %>%
      visPhysics(
        stabilization = list(
          enabled = TRUE, 
          iterations = 100
        ),
        solver = "forceAtlas2Based",
      )
  })
  ## XGBoost ####
  output$xg.fits.metrics<-renderPlotly({
    eval_log<-data.frame(eval_log)
    plot_ly(
      data = eval_log, 
      x = ~iter
    ) %>%
      add_trace(
        y = ~train_error, 
        name = 'Train Error', 
        mode = 'lines+markers',
        type="scatter",
        line = list(color = '#0d421f'),
        marker = list(color = '#0d421f')
      ) %>%
      add_trace(
        y = ~valid_error, 
        name = 'Validation Error',
        type="scatter",
        mode = 'lines+markers', 
        line = list(color = 'orange'),
        marker = list(color = 'orange')
      ) %>%
      layout(
        title = "Training and Validation Error",
        yaxis = list(
          title ="Error",
          tickfont = list(color = "#ffa500"), 
          titlefont = list(color = "#ffa500"),
          gridcolor = "rgba(0,0,0,0)"
        ),
        xaxis = list(
          title = "Iteration",
          tickfont = list(color = "#ffa500"), 
          titlefont = list(color = "#ffa500"),
          gridcolor = "rgba(0,0,0,0)"
        ),
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        plot_bgcolor = "rgba(0, 0, 0, 0)"
      )
  })
  output$xg.conf<-renderPlotly({
    confusion_plot(
      pred.round.train.xg,
      pred.round.test.xg,
      "Konfusionsdaten für Trainings- und Testdaten - xgBoost"
    )
  })

  output$network.xg<-renderVisNetwork({
    tmp<-xgb.model.dt.tree(
      feature_names = colnames(loan_data)[2:12], 
      model = xgb_mod.s,
      trees = input$xg.fits3-1)
    
    nodes <- tmp[, .(id = ID, label = Feature, value = Cover)]
    edges_yes <- tmp[!is.na(Yes), .(from = ID, to = Yes)]
    edges_no <- tmp[!is.na(No), .(from = ID, to = No)]
    
    nodes$shape<-NA
    nodes$shape[nodes$label=="Leaf"]<-"circle"
    nodes$shape[nodes$label!="Leaf"]<-"box"
    nodes$shape[1]<-"star"
    nodes$color<-NA
    nodes$color[nodes$label=="Leaf"]<-"#922ef0"
    nodes$color[nodes$label!="Leaf"]<-"orange"
    nodes$color[1]<-"red"
    
    nodes$label[nodes$label!="Leaf"]<-paste(
      1:length(nodes$label[nodes$label!="Leaf"]),
      nodes$label[nodes$label!="Leaf"]
    )
    nodes$label<-paste(
      nodes$label,
      "\nCover:",
      tmp$Cover %>% 
        round(digits = 2),
      "\nGain:",
      tmp$Quality %>% 
        round(digits = 2)
    )
    edges <- rbind(edges_yes, edges_no)
    edges <- edges[order(as.numeric(gsub("-", "", edges$to))), ]
    edges$label<-NA
    split<-na.omit(tmp$Split)
    yes<-na.omit(tmp$Yes)
    edges$label[edges$to %in% yes]<-split %>% 
      as.character()
    edges$font.color <- "orange"
    edges$color <- "#8105f5"
    visNetwork(nodes, edges) %>%
      visEdges(arrows = "to") %>%
      visNodes(font = list(color = "white")) %>%
      visOptions(
        highlightNearest = list(
          enabled = T, 
          degree = 1,
          hover=T
          ), 
        nodesIdSelection = TRUE
        ) %>%
      visLayout(
        randomSeed = 42, 
        improvedLayout = TRUE, 
        hierarchical = list(
          enabled = TRUE, 
          direction = 'UD', 
          sortMethod = 'directed')
        ) %>% 
      visPhysics(
        stabilization = list(
          enabled = TRUE, 
          iterations = 10
          ),
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravityConstant = 0,
          springLength = 200,
          springConstant = 0.1
          )
      )
  })
  # **Shapley Values** ####
  ## Info ####
  output$shap.ziel.xg<-renderPlotly({
    if(input$subdata.train=="A"){
      shap.Sum.Plot(
        x = if(input$subdata.pred=="A")
          pred.round.train.xg
        else
          tbltrain$loan_status,
        y = 1/(1+exp(-shap.xg.sum.train)),
        titel = "XG-Vorhersage <br> Trainingsdaten"
      )
    }else{
      shap.Sum.Plot(
        x = if(input$subdata.pred=="A")
          pred.round.test.xg
        else
          tbltest$loan_status,
        y = 1/(1+exp(-shap.xg.sum.test)),
        titel = "XG-Vorhersage <br> Trainingsdaten"
      )
    }
    
  })
  output$shap.ziel.nn<-renderPlotly({
    if(input$subdata.train=="A"){
      shap.Sum.Plot(
        x = if(input$subdata.pred=="A")
          pred.round.train.nn
        else
          tbltrain$loan_status,
        y = shap.nn.sum.train,
        titel = "NN-Vorhersage <br> Trainingsdaten"
      )
    }else{
      shap.Sum.Plot(
        x = if(input$subdata.pred=="A")
          pred.round.test.nn
        else
          tbltest$loan_status,
        y = shap.nn.sum.test,
        titel = "NN-Vorhersage <br> Trainingsdaten"
      )
    }
  })
  output$shap.ziel.lr<-renderPlotly({
    if(input$subdata.train=="A"){
      shap.Sum.Plot(
        x = if(input$subdata.pred=="A")
          pred.round.train.lr
        else
          tbltrain$loan_status,
        y = 1/(1+exp(-shap.lr.sum.train)),
        titel = "LR-Vorhersage <br> Trainingsdaten"
      )
    }else{
      shap.Sum.Plot(
        x = if(input$subdata.pred=="A")
          pred.round.test.lr
        else
          tbltest$loan_status,
        y = 1/(1+exp(-shap.lr.sum.test)),
        titel = "LR-Vorhersage <br> Trainingsdaten"
      )
    }
  })
  
  ## LR ####
  output$shap.lr.ind <- renderPlotly({
    if(input$shap.lr.slider)
      FeatureImportancePlot(shap.train.lr,norm.train,txt.legend.T)
    else
      FeatureImportancePlot(shap.test.lr,norm.test,txt.legend.F)
  })
  output$shap.lr.depend<-renderPlotly({
    if(input$shap.lr.slider)
      DependPlot(
        shap.data = shap.train.lr,
        norm.data = norm.train,
        pred.round = pred.round.train.lr,
        abzisse = input$shap.lr.slid.txt1,
        ordinate = input$shap.lr.slid.txt2
      )
    else
      DependPlot(
        shap.data = shap.test.lr,
        norm.data = norm.test,
        pred.round = pred.round.test.lr,
        abzisse = input$shap.lr.slid.txt1,
        ordinate = input$shap.lr.slid.txt2
      )
  })
  output$shap.lr.idividuell<-renderPlotly({
    BarPlot(
      rbind(shap.train.lr,shap.test.lr),
      rbind(norm.train,norm.test),
      c(pred.exakt.train.lr,pred.exakt.test.lr),
      input$user00.lr
    )
  })
  output$User.data.lr<-renderDT({
    data<-dt.funktion(shap.train.lr,shap.test.lr,input$user00.lr)
    dt <- datatable(
      data[-5],
      options = list(
        dom = 'tip',
        pageLength = 12,
        ordering = FALSE
      )
    )
    dt<-formatCurrency(dt,columns = 1:2, currency = '',  mark = '.', dec.mark = ',',digits = 0)
    formatCurrency(dt,columns = c(3:4), currency = '',  mark = '.', dec.mark = ',',digits = 4)
  })
  output$txt.lr<-renderUI({
    data<-dt.funktion(shap.train.lr,shap.test.lr,input$user00.lr)
    TextPrediction(data,input$user00.lr)
  })
  output$heat.shap.lr<-renderPlotly({
    HeatPlot(shap.train.lr,shap.test.lr,input$range.lr)
  })
  ## Neronale Netze ####
  output$shap.nn.ind <- renderPlotly({
    if(input$shap.nn.slider)
      FeatureImportancePlot(shap.train.nn,norm.train,txt.legend.T)
    else
      FeatureImportancePlot(shap.test.nn,norm.test,txt.legend.F)
  })
  output$shap.nn.depend<-renderPlotly({
    if(input$shap.nn.slider)
      DependPlot(
        shap.data = shap.train.nn,
        norm.data = norm.train,
        pred.round = pred.round.train.nn,
        abzisse = input$shap.lr.slid.txt1,
        ordinate = input$shap.lr.slid.txt2
      )
    else
      DependPlot(
        shap.data = shap.test.nn,
        norm.data = norm.test,
        pred.round = pred.round.test.nn,
        abzisse = input$shap.lr.slid.txt1,
        ordinate = input$shap.lr.slid.txt2
      )
  })
  output$shap.nn.idividuell<-renderPlotly({
    BarPlot(
      rbind(shap.train.nn,shap.test.nn),
      rbind(norm.train,norm.test),
      c(pred.exakt.train.nn,pred.exakt.test.nn),
      input$user00.nn
      )
  })
  output$User.data.nn<-renderDT({
    data<-dt.funktion(shap.train.nn,shap.test.nn,input$user00.nn)
    dt <- datatable(
      data[-5],
      options = list(
        dom = 'tip',
        pageLength = 12,
        ordering = FALSE
      )
    )
    dt<-formatCurrency(dt,columns = 1:2, currency = '',  mark = '.', dec.mark = ',',digits = 0)
    formatCurrency(dt,columns = c(3:4), currency = '',  mark = '.', dec.mark = ',',digits = 4)
  })
  output$txt.nn<-renderUI({
    data<-dt.funktion(shap.train.nn,shap.test.nn,input$user00.nn)
    TextPrediction(data,input$user00.nn)
  })
  output$heat.shap.nn<-renderPlotly({
    HeatPlot(shap.train.nn,shap.test.nn,input$range.nn)
  })
  ## XgBoost ####
  output$shap.xg.ind <- renderPlotly({
    if(input$shap.xg.slider)
      FeatureImportancePlot(shap.train.xg,norm.train,txt.legend.T)
    else
      FeatureImportancePlot(shap.test.xg,norm.test,txt.legend.F)
  })
 
  output$shap.xg.depend<-renderPlotly({
    if(input$shap.xg.slider)
      DependPlot(
        shap.data = shap.train.xg,
        norm.data = norm.train,
        pred.round = pred.round.train.xg,
        abzisse = input$shap.lr.slid.txt1,
        ordinate = input$shap.lr.slid.txt2
      )
    else
      DependPlot(
        shap.data = shap.test.xg,
        norm.data = norm.test,
        pred.round = pred.round.test.xg,
        abzisse = input$shap.lr.slid.txt1,
        ordinate = input$shap.lr.slid.txt2
      )
  })
  output$shap.xg.idividuell<-renderPlotly({
    BarPlot(
      shap.data = rbind(shap.train.xg,shap.test.xg),
      norm.data =  rbind(norm.train,norm.test),
      pred.exakt =  c(pred.exakt.train.xg,pred.exakt.test.xg),
      id = input$user00.xg
    )    
  })
  output$User.data.xg<-renderDT({
    data<-dt.funktion(shap.train.xg,shap.test.xg,input$user00.xg)
    dt <- datatable(
      data[-5],
      options = list(
        dom = 'tip',
        pageLength = 12,
        ordering = FALSE
      )
    )
    dt<-formatCurrency(dt,columns = 1:2, currency = '',  mark = '.', dec.mark = ',',digits = 0)
    formatCurrency(dt,columns = c(3:4), currency = '',  mark = '.', dec.mark = ',',digits = 4)
  })
  output$txt.xg<-renderUI({
    data<-dt.funktion(shap.train.xg,shap.test.xg,input$user00.xg)
    TextPrediction(data,input$user00.xg)
  })
  output$heat.shap.xg<-renderPlotly({
    HeatPlot(shap.train.xg,shap.test.xg,input$range.xg)
  })
# Funktionen ####
  txt.legend.T<-paste(
    "User:",tbltrain$loan_id ,"<br>",
    "Train:",T,"<br>",
    "Beobachtung:",tbltrain$loan_status,"<br>",
    "Vorhersage LR:",pred.round.train.lr,"<br>",
    "Vorhersage NN:",pred.round.train.nn,"<br>",
    "Vorhersage XG:",pred.round.train.xg,"<br>",
    "cibil_score:",tbltrain$cibil_score %>% round(digits = 2)  %>% f(),"<br>",
    "loan_term:",tbltrain$loan_term %>% round(digits = 2)  %>% f(),"<br>",
    "loan_amount:",tbltrain$loan_amount %>% round(digits = 2) %>% f(),"<br>",
    "bank_asset_value:",tbltrain$bank_asset_value %>% round(digits = 2) %>% f(),"<br>",
    "income_annum:",tbltrain$income_annum %>% round(digits = 2) %>% f(),"<br>",
    "commercial_assets_value:",tbltrain$commercial_assets_value %>% round(digits = 2) %>% f(),"<br>",
    "residential_assets_value:",tbltrain$residential_assets_value  %>% round(digits = 2) %>% f(),"<br>",
    "luxury_assets_value:",tbltrain$luxury_assets_value %>% round(digits = 2) %>% f(),"<br>",
    "no_of_dependents:",tbltrain$no_of_dependents %>% round(digits = 2) %>% f(),"<br>",
    "self_employed_No:",tbltrain$self_employed_No %>% round(digits = 2) %>% f(),"<br>",
    "education_Graduate:",tbltrain$education_Graduate %>% round(digits = 2) %>% f(),"<br>"
  )
  txt.legend.F<-paste(
    "User:",tbltest$loan_id ,"<br>",
    "Train:",T,"<br>",
    "Beobachtung:",tbltest$loan_status,"<br>",
    "Vorhersage LR:",pred.round.test.lr,"<br>",
    "Vorhersage NN:",pred.round.test.nn,"<br>",
    "Vorhersage XG:",pred.round.test.xg,"<br>",
    "cibil_score:",tbltest$cibil_score %>% round(digits = 2)  %>% f(),"<br>",
    "loan_term:",tbltest$loan_term %>% round(digits = 2)  %>% f(),"<br>",
    "loan_amount:",tbltest$loan_amount %>% round(digits = 2) %>% f(),"<br>",
    "bank_asset_value:",tbltest$bank_asset_value %>% round(digits = 2) %>% f(),"<br>",
    "income_annum:",tbltest$income_annum %>% round(digits = 2) %>% f(),"<br>",
    "commercial_assets_value:",tbltest$commercial_assets_value %>% round(digits = 2) %>% f(),"<br>",
    "residential_assets_value:",tbltest$residential_assets_value  %>% round(digits = 2) %>% f(),"<br>",
    "luxury_assets_value:",tbltest$luxury_assets_value %>% round(digits = 2) %>% f(),"<br>",
    "no_of_dependents:",tbltest$no_of_dependents %>% round(digits = 2) %>% f(),"<br>",
    "self_employed_No:",tbltest$self_employed_No %>% round(digits = 2) %>% f(),"<br>",
    "education_Graduate:",tbltest$education_Graduate %>% round(digits = 2) %>% f(),"<br>"
  )
 
  confusion_plot <- function(pred_train, pred_test, plot_title) {
    # Konfusionsdaten berechnen
    df_train <- caret::confusionMatrix(
      pred_train %>% factor(),
      norm.train$loan_status %>% factor()
    )$table %>%
      as.data.frame()
    df_test <- caret::confusionMatrix(
      pred_test %>% factor(),
      norm.test$loan_status %>% factor()
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
    
    p <- ggplot(
      df, 
      aes(
        x = Reference, 
        y = Freq, 
        fill = Prediction
        )
      ) +
      geom_bar(
        stat = "identity", 
        position = "stack"
      ) +
      scale_fill_manual(
        values = c("Abgelehnt" = "orange", "Genehmig" = "#0d421f")
        ) +
      geom_text(
        aes(label = label), 
        position = position_stack(vjust = 0.5),
        size = 5,
        colour = "indianred3"
      ) +
      facet_wrap(
        ~dataset, 
        scales = "free_y"
      ) +
      labs(
        x = "",
        y = "",
        fill = "Status",
        title = plot_title
      ) +
      theme(
        strip.text = element_text(size = 14, color = "orange"),
        strip.background = element_rect(fill = NA)
      )
    ggplotly(p) %>% layout(
      title =  list(
        font  = list(color = "#ffa500")
      ),
      yaxis = list(
        title ="Prediction",
        tickfont = list(color = "#ffa500"), 
        titlefont = list(color = "#ffa500"),
        gridcolor = "rgba(0,0,0,0)"
        
      ),
      yaxis2 = list(
        title ="Prediction",
        tickfont = list(color = "#ffa500"), 
        titlefont = list(color = "#ffa500"),
        gridcolor = "rgba(0,0,0,0)"
        
      ),
      xaxis = list(
        title = "Reference",
        tickfont = list(color = "#ffa500"), 
        titlefont = list(color = "#ffa500"),
        gridcolor = "rgba(0,0,0,0)"
      ),
      xaxis2 = list(
        title = "Reference",
        tickfont = list(color = "#ffa500"), 
        titlefont = list(color = "#ffa500"),
        gridcolor = "rgba(0,0,0,0)"
      ),
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      plot_bgcolor = "rgba(0, 0, 0, 0)",
      showlegend = FALSE
    )
  }
  
  shap.Sum.Plot<-function(x,y,titel){
    x.data<-x+rnorm(length(x),0,0.01)
    plot_ly(
      type = "scatter",
      mode= "markers",
      x = ~x.data,
      y = ~y,
      color =  ~if(length(x)==3415){
        tbltrain$loan_status==x
        }else{
          tbltest$loan_status==x
          },
      colors = c("orange","#0d421f"),
    ) %>%
      layout(
        xaxis = list(
          title = titel,
          showline = FALSE,
          zeroline = FALSE ,
          tickmode = "array",        
          tickvals = c(0, 1),       
          ticktext = c("Abgelehnt", "Genehmigt"),
          tickfont = list(color = "#ffa500"), 
          titlefont = list(color = "#ffa500")
        ),
        yaxis = list(
          title = "SHAP Sum",
          showline = FALSE,
          zeroline = FALSE,
          tickfont = list(color = "#ffa500"), 
          titlefont = list(color = "#ffa500")
        ),
        plot_bgcolor = "rgba(255, 255, 255, 0)",  
        paper_bgcolor = "rgba(255, 255, 255, 0)" 
      )
  }
  FeatureImportancePlot<-function(shap.data,norm.data,txt){
    long<-shap.plot.summary.wrap2(
      shap.data[,-12], 
      norm.data[,-12] , 
      top_n =11
      )
    unique.level<-unique(long$data$variable) %>% 
      as.character()
    n<-nrow(norm.data)
    long$data$y_values<-rnorm(nrow(long$data),0,0.05)
    p<-plot_ly(
      data = long$data,
      type = "scatter", 
      mode = "markers",
      hoverinfo = "text",
      text =txt,
      marker = list(
        colorscale=list(c(0, "orange"), c(1, "green")),
        cmin = -1,
        cmax = 1,
        showscale = TRUE, 
        colorbar = list(
          title = "Merkmal",
          tickvals = ~c(-1,1),
          ticktext = c("Hohe Ausprägung", "Niedrige Ausprägung")
        )
      )
    )
    
    for( i  in 1:11){
      p<-p %>% 
        add_trace(
          x = long$data$value[long$data$variable==unique.level[i]],
          y = i+long$data$y_values[1:n], 
          marker = list(
            color = if(
              cor(
                long$data$value[long$data$variable==unique.level[i]],
                long$data$rfvalue [long$data$variable==unique.level[i]]
              )>0
            ) long$data$value[long$data$variable==unique.level[i]]*-1
            else long$data$value[long$data$variable==unique.level[i]],
            size = 6
          ),
          name=unique.level[i]
        )
    }
    p %>%
      layout(
        yaxis = list(
          tickfont = list(color = "#ffa500"), 
          titlefont = list(color = "#ffa500"),
          gridcolor = "rgba(0,0,0,0)",
          tickvals = 1:11,
          ticktext = unique.level
        ),
        showlegend=F,
        xaxis = list(
          tickfont = list(color = "#ffa500"), 
          titlefont = list(color = "#ffa500"),
          gridcolor = "rgba(0,0,0,0)"
        ),
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        plot_bgcolor = "rgba(0, 0, 0, 0)"
      )
  }
  DependPlot<-function(shap.data,norm.data,pred.round,abzisse,ordinate){
    x<-norm.data[,abzisse]
    y<-shap.data[,ordinate]
    r<-pred.round==norm.data$loan_status
    loan_status<-norm.data$loan_status
    loess_fit <- loess(y ~ x)
    predicted_values <- predict(
      loess_fit, 
      data.frame(x = sort(x))
      )
    cor.test.data<-cor.test(x,y)
    filtered_data<-tibble(
      rfvalue = x,
      value = y,
      richtig = r,
      loan_status
      )
    filtered_data$richtig <- factor(
      filtered_data$richtig, 
      levels = c(TRUE, FALSE), 
      labels = c("Richtige Vorhersage", "Falsche Vorhersage")
    )
    filtered_data$id<-1:nrow(filtered_data)
    scatter<-plot_ly(
      data = filtered_data, 
      x = ~rfvalue, 
      y = ~value, 
      color = ~richtig,
      colors = c("#0d421f","orange"),
      type = "scatter", 
      text = ~paste(
        "USER:",id,"<br>",
        "loan_status:",loan_status
        ),
      mode = "markers") %>%
      add_trace(
        x = sort(filtered_data$rfvalue), 
        y = predicted_values, 
        mode = "lines", 
        type = "scatter",
        line = list(color = "orange"),
        showlegend=F,
        inherit = FALSE
      ) %>%
      add_annotations(
        x = ~median(rfvalue),
        y = ~max(value)*1.1,
        text = paste0(
          "R = ",
          cor.test.data$estimate %>% 
            round(digits = 2),
          ", p < ",
          ifelse(
            cor.test.data$p.value %>% 
              round(4)==0,
            "2.2e-16",
            cor.test.data$p.value %>% 
              round(4)
            )
          ),
        showarrow = FALSE,
        font = list(size = 16, color = "orange")
      ) %>%
      layout(
        yaxis = list(
          title= paste("Shapley Value"),
          tickfont = list(color = "#ffa500"), 
          titlefont = list(color = "#ffa500"),
          gridcolor = "rgba(0,0,0,0)"
        ),
        xaxis = list(
          title= paste("Referenz Wert",abzisse),
          tickfont = list(color = "#ffa500"), 
          titlefont = list(color = "#ffa500"),
          gridcolor = "rgba(0,0,0,0)"
        )
      )
    # Histogramme
    hist_x <- plot_ly(
      data = filtered_data,
      x = ~rfvalue,
      type = "histogram",
      histnorm = "probability",
      marker = list(color = "orange"),
      name = abzisse
    ) %>%
      layout(bargap=0.1)
    
    hist_y <- plot_ly(
      data = filtered_data,
      y = ~value,
      type = "histogram",
      marker = list(color = "#0d421f"),
      histnorm = "probability",
      name = paste("Shap",ordinate)
      )%>%
      layout(bargap=0.1)
    empty_plot <- plotly_empty(type="scatter",mode="markers")
    
    subplot(
      hist_x,
      empty_plot,
      scatter,
      hist_y,
      nrows = 2,
      shareX = TRUE,
      shareY = TRUE,
      heights = c(0.2, 0.8),
      widths  = c(0.8, 0.2)
    ) %>%
      layout(
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        plot_bgcolor = "rgba(0, 0, 0, 0)"
      )
  }
  BarPlot<-function(shap.data,norm.data,pred.exakt,id){
    id<-as.numeric(id)
    data <- data.frame(
      value = shap.data[id,-12] %>% 
        t() 
    )
    data$variable<-colnames(norm.data[,-12])
    colnames(data)[1]<-"value"
    ggplot(
      data, 
      aes(
        x = value, 
        y = variable, 
        fill = value > 0
        )
      ) + 
      geom_col() +
      scale_fill_manual(
        values = c("TRUE" = "green", "FALSE" = "red")
        ) +
      labs(
        title = "Individual SHAP Values ",
        x = "Variable",
        y = "SHAP Value"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, color = "orange"),
        axis.text.y = element_text(color = "orange"),
        axis.title.x = element_text(color = "orange"),
        axis.title.y = element_text(color = "orange"),
        plot.title = element_text(color = "orange"),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        strip.text.x = element_text(color = "orange"),
        strip.text.y = element_text(color = "orange"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none"
      ) 
  }
  dt.funktion<-function(shap.data.train,shap.data.test,User){
    # Featureimportance
    means.shap<-rbind(shap.data.train,shap.data.test) %>%
      abs() %>%
      colMeans()
    # individuelle Daten
    indiv.shap<-rbind(shap.data.train,shap.data.test)[User,] 
    # Durchschnittswerte
    means.allg<-rbind(tbltrain[-1],tbltest[-1]) %>%
      colMeans()
    # individuelle Shap Values
    indiv.allg<-rbind(tbltrain,tbltest)[User,-1]
    data<-rbind(
      indiv.allg[-12],
      means.allg[-12],
      indiv.shap[-12],
      means.shap[-12]
      ) %>% 
      t() %>% 
      data.frame()
    
    colnames(data)<-c(
      paste("User",User,"Allgemein"),
      "Mean Userdaten Allg.",
      paste("User",User,"SHAP"),
      "Mean Shap Allg"
    )
    data$shap.wichtigkeit<-data[,3] %>% 
      abs() %>% 
      rank() 
    
    data<-data %>%
      arrange(desc(shap.wichtigkeit))
    return(data)
  }
  TextPrediction<-function(data,user){
    HTML(
      paste(
        tags$div(
          "Für den User",
          user, 
          "hat das Merkmal",
          rownames(data)[1], 
          "mit einem Wert von",
          data[1,1] %>% 
            round(3) %>% 
            f(),
          "den größten Einluss auf die Prognose. Der",
          ifelse(data[1,3]>0,"positive","negative") ,
          "Shapley Wert von ",
          data[1,3] %>% 
            round(3),
          "führt zu einer ",
          ifelse(data[1,3]>0,"Zusage","Ablehnung"), 
          "des Kreditantrags.
          Das zweitwichtigste Merkmal",
          rownames(data)[2], 
          "liegt",
          ifelse(data[2,1]>data[2,2],"über","unter"),
          "dem Mittelwert aller Kundendaten. Der",
          ifelse(data[2,3]>0,"positive","negative") ,
          "Shapley Wert von ",
          data[2,3] %>% 
            round(3),
          ifelse(
            data[2,3]>0 & data[1,3]>0 |data[2,3]<0 & data[1,3]<0,
            "unterstützt die Prognose.",
            paste(
              "würde hingegen ",
              ifelse(data[2,3]>0,"für","gegen"),
              "die Prognose stimmen." 
              )
          ),
          "Das drittwichtigste Merkmal",
          rownames(data)[3],
          "hat bereits deutlich weniger aber immer noch einen Einfluss auf die Prognose.",
          style = "font-size: 18px;"
        )
      )
    )
  }
  
  HeatPlot<-function(shap.data.train,shap.data.test,n){
    dat <-rbind(shap.data.train,shap.data.test)[(n-99):n,]
    plot_ly(
      x = (n-99):n,
      y = colnames(dat),
      z = as.matrix(dat) %>% t(),
      colors = colorRamp(c("orange", "#0d421f")),
      type = "heatmap"
    ) %>%
      layout(
        title = paste("Heatmap SHapley Werte für 100 Userdaten"),
        xaxis = list(
          tickfont = list(color = "#ffa500"), 
          titlefont = list(color = "#ffa500"),
          gridcolor = "rgba(0,0,0,0)"),
        yaxis = list(
          tickfont = list(color = "#ffa500"), 
          titlefont = list(color = "#ffa500"),
          gridcolor = "rgba(0,0,0,0)"),
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        plot_bgcolor = "rgba(0, 0, 0, 0)"
      )
  }
  
  # Kreditabfrage ####
  input_Name <- reactiveVal("Meier")
  input_Herr <- reactiveVal("Herr")
  input_no_of_dependents <- reactiveVal(2)
  input_education_Graduate <- reactiveVal(0)
  input_self_employed_No<- reactiveVal(0)
  input_residential_assets_value<- reactiveVal(10000000)
  input_commercial_assets_value<- reactiveVal(10000000)
  input_luxury_assets_value<- reactiveVal(10000000)
  input_bank_asset_value<- reactiveVal(10000000)
  input_income_annum<- reactiveVal(10000000)
  input_loan_term <- reactiveVal(2)
  input_loan_amount <- reactiveVal(10000000)
  input_cibil_score<- reactiveVal(sample(300:900,1))
  simPredict<-reactiveVal(NULL)
  btn1<-reactiveVal(NULL)
  btn2<-reactiveVal(NULL)
  btn3<-reactiveVal(NULL)
  btn4<-reactiveVal(NULL)
  btn5<-reactiveVal(F)
  ## Button 1 Name####
  observeEvent(input$btnSimu, {
    shinyalert(
      html = TRUE, 
      text = as.character(
        tagList(
          textInput(
            "sim.name", 
            h2("Bitte geben Sie Ihren Namen ein:"), 
            value = "Meier"
            ),
          radioButtons(
            inputId = 'sim.herr', 
            label = '', 
            choices = c("Herr","Frau"),
            inline = TRUE
          )
        )
      ),
      confirmButtonText = "Weiter",
      showCancelButton = TRUE,
      cancelButtonText = "Beenden",
      callbackR = function(value) {
        if (value) {
          btn1(value)
        }
      }
    )
    
    
  })
  ## Button 2 Allgemeine Info ####
  observeEvent(btn1(), {
    if(btn1()==T){
      shinyalert(
        html = TRUE, 
        text = as.character(
          tagList(
            h1(
              paste0(
                "Hallo ",
                input$sim.herr,
                " ",
                input$sim.name,
                ", wir brauchen zunächst ein paar allgemeine Informationen von Ihnen."
              )
            ),
            radioButtons(
              inputId = 'sim.kinder', 
              label = 'Wie viele Kinder leben in Ihrem Haushalt?', 
              choices = 0:5,
              inline = TRUE
            ),
            radioButtons(
              inputId = 'sim.hochschule', 
              label = 'Haben Sie einen Hochschulabschluss?', 
              choices = c("Ja"=1,"Nein"=1),
              inline = TRUE
            ),
            radioButtons(
              inputId = 'sim.selbstständig', 
              label = 'Sind Sie selbstständig?', 
              choices = c("Ja"=1,"Nein"=0),
              inline = TRUE
            )
          )
        ),
        showCancelButton = TRUE,
        confirmButtonText = "Weiter zur finanziellen Situation",
        cancelButtonText = "Abbrechen",
        callbackR = function(value) {
          if (value) {
            btn2(value)
          }
        }
      )
    }else{
      btn1(NULL)
    }
  })
  ## Button 3 Vermögenswerte ####
  observeEvent(btn2(), {
    if(btn2()==T){
      shinyalert(
        html = TRUE, 
        text = as.character(
          tagList(
            selectInput(
              inputId = 'sim_residential', 
              label = 'Wert des Wohnvermögens?', 
              choices = c(seq(0,1000000,100000),seq(2000000,30000000,1000000)) %>% f(),
              selected = (c(seq(0,1000000,100000),seq(2000000,30000000,1000000)) %>% f())[20]
            ),
            
            selectInput(
              inputId = 'sim_commerc', 
              label = 'Wert des Geschäftsvermögen?', 
              choices = c(seq(0,1000000,100000),seq(2000000,20000000,1000000)) %>% f(),
              selected = (c(seq(0,1000000,100000),seq(2000000,20000000,1000000)) %>% f())[20]
            ),
            
            selectInput(
              inputId = 'sim.luxus', 
              label = 'Wert der Luxusgütern?', 
              choices = c(seq(0,1000000,100000),seq(2000000,40000000,1000000)) %>% f(),
              selected = (c(seq(0,1000000,100000),seq(2000000,40000000,1000000)) %>% f())[20]
            ),
            
            selectInput(
              inputId = 'sim.bank', 
              label = 'Wert des Bankvermögens?', 
              choices = c(seq(0,1000000,100000),seq(2000000,15000000,1000000)) %>% f(),
              selected = (c(seq(0,1000000,100000),seq(2000000,15000000,1000000)) %>% f())[5]
            ),
            
            selectInput(
              inputId = 'sim.income', 
              label = 'Wie hoch ist Ihr Einkommen?', 
              choices = c(seq(0,1000000,100000),seq(2000000,10000000,1000000)) %>% f(),
              selected = (c(seq(0,1000000,100000),seq(2000000,10000000,1000000)) %>% f())[1]
            )
          )
        ),
        showCancelButton = TRUE,
        confirmButtonText = "Weiter zu den Kreditdaten",
        cancelButtonText = "Abbrechen",
        callbackR = function(value) {
          if (value) {
            btn3(value)
          }
        }
      )
    }else{
      btn1(NULL)
      btn2(NULL)
    }
  })
  ## Button 4 Kreditinfos ####
  observeEvent(btn3(), {
    if(btn3()){
      shinyalert(
        html = TRUE, 
        text = as.character(
          tagList(
            h1(
              "Auf Basis Ihrer Daten konnten wir ein Kreditscore von",
              input_cibil_score() ,
              "ermittelt." 
            ),
            radioButtons(
              inputId = 'sim_loan_term', 
              label = 'Wie lang soll die Kreditlaufzeit sein?', 
              choices = seq(2,20,2),
              selected = 4,
              inline = F
            ),
            selectInput(
              inputId = 'sim_loan_amount', 
              label = 'Kredit Höhe?', 
              choices = c(seq(0,1000000,100000),seq(2000000,15000000,1000000)) %>% f(),
              selected = (c(seq(0,1000000,100000),seq(2000000,15000000,1000000)) %>% f())[5]
            )
          )
        ),
        showCancelButton = TRUE,
        confirmButtonText = "Simulation starten",
        cancelButtonText = "Abbrechen",
        callbackR = function(value) {
          if (value) {
            btn4(value)
          }
        }
      )
    }else{
      btn1(NULL)
      btn2(NULL)
      btn3(NULL)
    }
  })
  ## Aktualisiere Input ####
  observeEvent(input$sim.name, {
    input_Name(input$sim.name)
  })
  observeEvent(input$sim.herr, {
    input_Herr(input$sim.herr)
  })
  observeEvent(input$sim.kinder, {
    input_no_of_dependents(
      input$sim.kinder %>% 
        as.numeric()
      )
  })
  observeEvent(input$sim.hochschule, {
    input_education_Graduate(
      input$sim.hochschule %>% 
        as.numeric()
      )
  })
  observeEvent(input$sim.selbstständig, {
    input_self_employed_No(
      input$sim.selbstständig %>% 
        as.numeric()
      )
  })
  observeEvent(input$sim_residential, {
    input_residential_assets_value (
      gsub("\\.","",input$sim_residential) %>% 
        as.numeric()
      )
  })
  observeEvent(input$sim_commerc, {
    input_commercial_assets_value(
      gsub("\\.","",input$sim_commerc) %>% 
        as.numeric()
      )
  })
  observeEvent(input$sim.luxus, {
    input_luxury_assets_value(
      gsub("\\.","",input$sim.luxus) %>% 
        as.numeric()
      )
  })
  observeEvent(input$sim.bank, {
    input_bank_asset_value(
      gsub("\\.","",input$sim.bank) %>% 
        as.numeric())
  })
  observeEvent(input$sim.income, {
    input_income_annum(
      gsub("\\.","",input$sim.income) %>% 
        as.numeric())
  })
  observeEvent(input$sim_loan_term, {
    input_loan_term(
      as.numeric(input$sim_loan_term)
      )
  })
  observeEvent(input$sim_loan_amount, {
    input_loan_amount(
      gsub("\\.","",input$sim_loan_amount) %>% 
        as.numeric())
  })
  ## Button 5 Bearbeitung erfolgreich####
  observeEvent(btn4(), {
    if(btn4()){
      shinyalert(
        html = TRUE, 
        text = tagList(
          h1("Wir haben Ihre Daten erfolgreich verarbeitet.")
        ),
        confirmButtonText = "Zum Ergebnis",
        callbackR = function(value) {
          if (value) {
            btn5(value)
          }
        }
      )
    }
  })
  ## Berechne Prognose ####
  observeEvent(btn5(), {
    if(btn5()==T){
      # Start mit erster Instanz, danach überschreiben:
      tmp<-rbind(tbltrain[-1],tbltest[-1])[1,] 
      tmp$no_of_dependents<-input_no_of_dependents()
      tmp$income_annum<-input_income_annum()
      tmp$loan_amount<-input_loan_amount()
      tmp$loan_term<-input_loan_term()
      tmp$cibil_score<-input_cibil_score()
      tmp$residential_assets_value<-input_residential_assets_value()
      tmp$commercial_assets_value<-input_commercial_assets_value()
      tmp$luxury_assets_value<-input_luxury_assets_value()
      tmp$bank_asset_value<-input_bank_asset_value()
      tmp$education_Graduate<-input_education_Graduate()
      tmp$self_employed_No<-input_self_employed_No()
      # normalisieren
      normalize <- function(x, min_values, max_valvalues) {
        return((x - min_values) / (max_valvalues - min_values))
      }
      min_vals <- sapply(tbltrain %>% dplyr::select(-loan_id), min)
      max_vals <- sapply(tbltrain %>% dplyr::select(-loan_id), max)
      tmp<-as.data.frame(
        mapply(
          function(x, min_val, max_val) 
            normalize(x, min_val, max_val), 
          tmp, 
          min_vals, 
          max_vals, 
          SIMPLIFY = FALSE)
      )
      xgtest <- xgb.DMatrix(as.matrix(tmp %>% dplyr::select(-loan_status)))
      p<- predict(xgb_mod.s,xgtest)
     
      output$simulation <- renderUI({
        tags$div(
          HTML(
            paste0(
              h1(
                ifelse(
                  input_Herr()=="Herr",
                  "Sehr geehrter Herr ",
                  "Sehr geehrte Frau "
                  ),
                input_Name(),
                ","
              ),
              br(),
              ifelse(
                p>.5,
                "Die Chancen für eine Kreditgenehmigung stehen gut. ",
                "Mit den Daten werden Sie wahrscheinlich keinen Kredit erhalten."),
              br(),
              br(),
              "Hier sehen Sie, wie sich unser Ergebnis zusammensetzt:",
              br(),
              br(),
              hr()
            )
          ),
          style = "font-size: 18px;"
        )
      })
      output$titleSlider<-renderUI({
        h2("Wie verändert sich die Prognose?")
      })
      ## Slider erstellen ####
      output$sim.s1<-renderUI({
        sliderInput(
          "slid.no_of_dependents",
          label = "Anzahl der Kinder",
          min = 0,
          max=5,
          value = input_no_of_dependents()
          )
      })
      output$sim.s2<-renderUI({
        sliderInput(
          "slid.loan_amount",
          label = "Kredithöhe",
          min = 0,
          max=40000000,
          step = 1000000,
          value = input_loan_amount(),
          width = "100%"
          )
      })
      output$sim.s3<-renderUI({
        sliderInput(
          "slid.loan_term",
          label = "Kreditlaufzeit",
          min = 2,
          max=20,
          step = 2,
          value = input_loan_term()
          )
      })
      output$sim.s4<-renderUI({
        sliderInput(
          "slid.cibil_score",
          label = "Kreditscore",
          min = 300,
          max=900,
          step = 1,
          value = input_cibil_score()
          )
      })
      output$sim.s5<-renderUI({
        sliderInput(
          "slid.residential_assets_value",
          label = "Wohnimmobilienwert",
          min = 0,
          max=30000000,
          step = 1000000,
          value = input_residential_assets_value()
          )
      })
      output$sim.s6<-renderUI({
        sliderInput(
          "slid.commercial_assets_value",
          label = "Geschäftsvermögen",
          min = 0,
          max=20000000,
          step = 1000000,
          value = input_commercial_assets_value()
          )
      })
      output$sim.s7<-renderUI({
        sliderInput(
          "slid.luxury_assets_value",
          label = "Luxusgüter",
          min = 0,
          max=40000000,
          step = 1000000,
          value = input_luxury_assets_value()
          )
      })
      output$sim.s8<-renderUI({
        sliderInput(
          "slid.bank_asset_value",
          label = "Bankvermögen",
          min = 0,
          max=15000000,
          step = 1000000,
          value = input_luxury_assets_value()
          )
      })
      output$sim.s9<-renderUI({
        radioButtons(
          "slid.education_Graduate",
          label = "Hochschulabschluss",
          choices = c(T=1,F=0),
          selected  = input_education_Graduate(),
          inline = T
          )
      })
      output$sim.s10<-renderUI({
        radioButtons(
          "slid.self_employed_No",
          label = "Selbstständig",
          choices = c(T=1,F=0),
          selected  = input_self_employed_No(),
          inline = T
          )
      })
      output$sim.s11<-renderUI({
        sliderInput(
          "slid.income_annum",
          label = "Bankvermögen",
          min = 0,
          max=10000000,
          step = 100000,
          value = input_income_annum()
          )
      })
    }
  })
  
  observeEvent(input$slid.income_annum,{
    output$sim.p2<-renderPlotly({
      values<-sim.shap()   
      features <- c(colnames(loan_data)[2:12],"BIAS")
      cumulative_values <- cumsum(values[c(12,1:11)] %>% as.vector())
      cumulative_values<-1/(1+exp(-cumulative_values))
      names(cumulative_values)<-c("BIAS",features[-12])
      # Kreise vorbereiten
      arrow_x <- cumulative_values
      arrow_y <- 12:1
      base_x <- c(.5, cumulative_values) %>% unlist()
      base_y <- 12:0
      
      ggplot() +
        annotate(
          "segment",
          x = base_x[-length(base_x)],
          xend = base_x[-1],
          y = base_y[-13],
          yend = base_y[-13],
          arrow = arrow(type = "closed", length = unit(0.05, "inches")),
          color = ifelse(diff(c(base_x)) > 0, "green", "red"),
          size = 2  
        )+
        geom_text(
          aes(
            x = base_x[-length(base_x)], 
            y = base_y[-13]+.2, 
            label =
              ifelse(
                diff(c(base_x))>0,
                paste(
                  "+", 
                  diff(base_x) %>% 
                    round(3) %>% 
                    as.character()
                  ),
                diff(base_x) %>%
                  round(3) %>%
                  as.character()
              )
          ),
          size=3.5,
          color="gray"
        ) +
        scale_y_continuous(
          name = "", 
          breaks = 12:0, 
          labels = paste(
            c(names(base_x)[2:13],"Endwert"),
            "=",
            cumulative_values %>% 
              round(3)
          )
        ) +
        labs(title = "Kummulierte  Shapley Values",x = "") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, color = "orange"),
          axis.text.y = element_text(color = "orange"),
          axis.title.x = element_text(color = "orange"),
          axis.title.y = element_text(color = "orange"),
          plot.title = element_text(color = "orange"),
          panel.background = element_rect(fill = "black"),
          plot.background = element_rect(fill = "black"),
          strip.text.x = element_text(color = "orange"),
          strip.text.y = element_text(color = "orange"),
          panel.grid.major = element_blank(),  
          panel.grid.minor = element_blank()
        )
    })
    
    output$sim.p1<-renderPlotly({
      xg.shap<-rbind(shap.train.xg,shap.test.xg)
      tmpo<-sim.shap()  
      data <- data.frame(
        value = tmpo[-12],
        cor = cor(
          rbind(norm.train,norm.test)[,c(-1,-13)],
          xg.shap[,-12]
          ) %>% 
          diag()
      )
      data<-tmpo[c(12,1:11)] %>%
        data.frame()
      data$variable<-factor(
        c(
          "BIAS",
          colnames(loan_data[,c(-1,-13)])
          ),
        levels = rev(
          c(
            "BIAS",
            colnames(loan_data[,c(-1,-13)])
            )
          )
        )
      colnames(data)[1]<-"value"
      ggplot(
        data, 
        aes(x = value, y = variable, fill = value > 0)) + 
        geom_col() +
        scale_fill_manual(
          values = c("TRUE" = "green", "FALSE" = "red")
          ) +
        labs(
          title = "Individual SHAP Values ",
          x = "Variable",
          y = "SHAP Value"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, color = "orange"),
          axis.text.y = element_text(color = "orange"),
          axis.title.x = element_text(color = "orange"),
          axis.title.y = element_text(color = "orange"),
          plot.title = element_text(color = "orange"),
          panel.background = element_rect(fill = "black"),
          plot.background = element_rect(fill = "black"),
          strip.text.x = element_text(color = "orange"),
          strip.text.y = element_text(color = "orange"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "none"
        ) 
    })
  })
  ### indiv Shapley Values ####
  sim.shap<-reactive({
    tmp<-rbind(tbltrain[-1],tbltest[-1])[1,]
    tmp$no_of_dependents<-input$slid.no_of_dependents %>% as.numeric()
    tmp$income_annum<-input$slid.income_annum%>% as.numeric()
    tmp$loan_amount<-input$slid.loan_amount
    tmp$loan_term<-input$slid.loan_term%>% as.numeric()
    tmp$cibil_score<-input$slid.cibil_score %>% as.numeric()
    tmp$residential_assets_value<-input$slid.residential_assets_value%>% as.numeric()
    tmp$commercial_assets_value<-input$slid.commercial_assets_value%>% as.numeric()
    tmp$luxury_assets_value<-input$slid.luxury_assets_value%>% as.numeric()
    tmp$bank_asset_value<-input$slid.bank_asset_value%>% as.numeric()
    tmp$education_Graduate<-input$slid.education_Graduate %>% as.numeric()
    tmp$self_employed_No<-input$slid.self_employed_No %>% as.numeric()
    normalize <- function(x, min_values, max_valvalues) {
      return((x - min_values) / (max_valvalues - min_values))
    }
    min_vals <- sapply(tbltrain %>% dplyr::select(-loan_id), min)
    max_vals <- sapply(tbltrain %>% dplyr::select(-loan_id), max)
    tmp<-as.data.frame(
      mapply(
        function(x, min_val, max_val) 
          normalize(x, min_val, max_val), 
        tmp, 
        min_vals, 
        max_vals, 
        SIMPLIFY = FALSE)
    )
    xgtest <- xgb.DMatrix(as.matrix(tmp %>% dplyr::select(-loan_status)))
    return(predict(xgb_mod.s,xgtest, predcontrib = TRUE))
  })

  
}
