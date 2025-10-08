server <- function(input, output, session) {
  data_secteurs <- dbGetQuery(con, "SELECT id, nom, nb_pecheur_secteur FROM secteurs ORDER BY nom")
  data_especes <- dbGetQuery(con, "SELECT id, nom FROM especes_peche ORDER BY nom")
  
  
  shape <- st_read("shape/BNDA_MDG_2000-01-01_2004-04-16.shp")
  shape_village <- st_read("shape/Village_CORECRABE/Village_CORECRABE.shp")
  shape_village_belo <- shape_village %>%
    mutate(VILLAGE_UP = toupper(VILLAGE)) %>%
    filter(VILLAGE_UP %in% data_secteurs$nom)%>%
      left_join(
        data_secteurs,
        by = c("VILLAGE_UP" = "nom")
      )
  #shape_village_belo <- shape_village_belo %>%
    #filter(!is.na(nb_pecheur_secteur))
  shape_village_belo <- st_transform(shape_village_belo, 4326)
  shape_belon <- shape %>%
    filter(adm2nm == "Belon'i Tsiribihina")
  mangrove = st_read(dsn ="./shape/Mangrove_Mada/Mangroves_IEFN.shp")
  #View(mangrove)
  
  firstQuery  <-  paste0(
    "SELECT *
       FROM repartition_par_secteur"
  )
  firstData <- dbGetQuery(con, firstQuery)
  selected_village <- reactiveVal(NULL)
  data_selected_village <- reactiveVal(firstData)
  
  observeEvent(input$indicateurs, {
    # Code exécuté à chaque fois que la sélection change
    if(input$indicateurs == "Démographie de la communauté des pêcheurs"){
      query  <-  paste0(
        "SELECT *
          FROM repartition_par_secteur"
      )
    }else if(input$indicateurs == "Proportions de pêcheurs enquêtés par filières halieutiques"){
      query  <-  paste0(
        "SELECT *
          FROM repartition_par_filiere"
      )
    }
    
    data <- dbGetQuery(con, query)
    if(!is.null(selected_village())){
      data <- data %>%
        filter(secteurid == selected_village())
    }
    data_selected_village(data)
  })
  
  observeEvent(input$map_placeholder_marker_click, {
    click <- input$map_placeholder_marker_click
    
    if (!is.null(click$id) && startsWith(click$id, "village_")) {
      village_clique <- sub("^village_", "", click$id)
      selected_village(village_clique)
      if(input$indicateurs == "Démographie de la communauté des pêcheurs"){
        query  <-  paste0(
          "SELECT *
          FROM repartition_par_secteur"
        )
      }else if(input$indicateurs == "Proportions de pêcheurs enquêtés par filières halieutiques"){
        query  <-  paste0(
          "SELECT *
          FROM repartition_par_filiere"
        )
      }
      
      data <- dbGetQuery(con, query)
      data <- data %>%
        filter(secteurid == village_clique)
      data_selected_village(data)
      View(data_selected_village())
      #print(paste("Village cliqué :", village_clique))
    }
  })
  
  observeEvent(input$map_placeholder_shape_click, {
    click <- input$map_placeholder_shape_click
    if(input$indicateurs == "Démographie de la communauté des pêcheurs"){
      query  <-  paste0(
        "SELECT *
          FROM repartition_par_secteur"
      )
    }else if(input$indicateurs == "Proportions de pêcheurs enquêtés par filières halieutiques"){
      query  <-  paste0(
        "SELECT *
          FROM repartition_par_filiere"
      )
    }
    data <- dbGetQuery(con, query)
    selected_village(NULL)
    data_selected_village(data)
  })
  
  output$text_village_selected <- renderText({
    if(!is.null(selected_village())){
      nom <- shape_village_belo %>%
        filter(id == selected_village()) %>%
        pull(VILLAGE)
      paste(nom)
    }else{
      paste("Toutes les secteurs")
    }
    
  })
  
  output$indicateurs <- renderText({
      paste(input$indicateurs)
  })
  
  output$legende <- renderText({
    if(!is.null(selected_village())){
      nom <- shape_village_belo %>%
        filter(id == selected_village()) %>%
        pull(VILLAGE)
    }else{
      nom<-"toutes les secteurs"
    }
    
    paste(" La figure représente le nombre de pêcheurs de ", nom , " toutes filières confondues, ainsi que la proportion d'hommes et de femmes au sein de ces communautés de pêcheurs.")
    
  })
  
  output$result_map_click <- renderPlotly({
    
      data_func <- data_selected_village()
      
      data_avec_total <- data_func%>%
        left_join(
          data_secteurs,
          by = c("secteurid" = "id")
        )
      
      if(is.null(selected_village)){
        
        if(input$indicateurs == "Démographie de la communauté des pêcheurs"){
          data_avec_total <- data_avec_total %>%
            summarise(
              feminin = sum(feminin, na.rm = TRUE),
              masculin = sum(masculin, na.rm = TRUE),
              nb_pecheur_secteur = sum(nb_pecheur_secteur, na.rm = TRUE)
            )
        }else if(input$indicateurs == "Proportions de pêcheurs enquêtés par filières halieutiques"){
          data_avec_total <- data_avec_total %>%
            summarise(
              Chevaquine = sum(Chevaquine, na.rm = TRUE),
              Crabe = sum(Crabe, na.rm = TRUE),
              Crevette = sum(Crevette, na.rm = TRUE),
              Gros_poisson = sum(Gros_poisson, na.rm = TRUE),
              Petit_poisson = sum(Petit_poisson, na.rm = TRUE),
              Poisson_courbine = sum(Poisson_courbine, na.rm = TRUE)
            )
        }
      }
      if(input$indicateurs == "Démographie de la communauté des pêcheurs"){
        data_avec_total <- data_avec_total %>%
        mutate(non_enquete = nb_pecheur_secteur - feminin - masculin)
      
        df_long <- data_avec_total %>%
        select(feminin, masculin, non_enquete) %>%
        pivot_longer(everything(), names_to = "categorie", values_to = "nombre")
      }else if(input$indicateurs == "Proportions de pêcheurs enquêtés par filières halieutiques"){
        
        df_long <- data_avec_total %>%
          select(Chevaquine, Crabe, Crevette, Gros_poisson,Petit_poisson, Poisson_courbine) %>%
          pivot_longer(everything(), names_to = "categorie", values_to = "nombre")
      }
    
    plot_ly(
      df_long,
      labels = ~categorie,
      values = ~nombre,
      type = 'pie',
      hoverinfo = 'label+value+percent',
      marker = list(colors = c('#e887d4', '#2596be', '#eeeee4'))
    ) %>%
      layout(
        showlegend = TRUE,
        paper_bgcolor = "rgba(240, 248, 255, 1)",  # bleu très clair (canvas)
        plot_bgcolor = "rgba(255, 255, 255, 0)", 
        margin = list(
          l = 40,  # left
          r = 40,  # right
          b = 40,  # bottom
          t = 80,  # top
          pad = 20 # (optionnel) espace interne
        )
      )
    
  })
  
  
  output$map_placeholder <- renderLeaflet({
    leaflet(shape_belon) %>%
      addTiles() %>% 
      setView( lng = 44.7009372, lat = -19.7045099, zoom = 10 )%>%
      addPolygons(
        color = "blue", 
        weight = 1, 
        fillColor = "lightblue", 
        fillOpacity = 0.5,
        popup = ~as.character(adm2nm) # remplace NOM_COLONNE par un champ du shapefile
      )%>%
      addPolygons(
        data = mangrove,
        color = "green", 
        weight = 0.5, 
        fillColor = "green", 
        fillOpacity = 0.5 # remplace NOM_COLONNE par un champ du shapefile
      )%>%
      addCircleMarkers(
        data = shape_village_belo,
        radius = 15,   # met à l’échelle pour éviter des cercles trop gros
        color = "red",
        fillOpacity = 0.7,
        layerId = ~paste0("village_", id),
        popup = ~paste0(VILLAGE, "<br>Nombre pêcheurs : ", nb_pecheur_secteur) # remplace NOM_COLONNE par le champ qui contient le nom du village
      )
  })
  
  # ===== NETTOYAGE =====
  onStop(function() {
    dbDisconnect(con)
  })
}