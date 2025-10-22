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
    }else if(input$indicateurs == "Proportions de pêcheurs enquêtés par techniques de pêches"){
      query  <-  paste0(
        "SELECT *
          FROM repartition_par_technique"
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
      }else if(input$indicateurs == "Proportions de pêcheurs enquêtés par techniques de pêches"){
        query  <-  paste0(
          "SELECT *
          FROM repartition_par_technique"
        )
      }
      
      data <- dbGetQuery(con, query)
      data <- data %>%
        filter(secteurid == village_clique)
      data_selected_village(data)
      #View(data_selected_village())
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
    }else if(input$indicateurs == "Proportions de pêcheurs enquêtés par techniques de pêches"){
      query  <-  paste0(
        "SELECT *
          FROM repartition_par_technique"
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
      paste("Tous les secteurs")
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
      nom<-"tous les secteurs"
    }
    if(input$indicateurs == "Démographie de la communauté des pêcheurs"){
      paste(" La figure représente le nombre de pêcheurs de ", nom , " toutes filières confondues, ainsi que la proportion d'hommes et de femmes au sein de ces communautés de pêcheurs.")
    }else if(input$indicateurs == "Proportions de pêcheurs enquêtés par filières halieutiques"){
      paste(" La figure représente la proportion de pêcheurs de ", nom , " impliqués dans chaque filière (Chevaquine, Crabe, Crevette, Gros poissons, Petit poissons ou Courbine) ")
    }else if(input$indicateurs == "Proportions de pêcheurs enquêtés par techniques de pêches"){
      paste(" La figure représente la proportion de pêcheurs de ", nom , " impliqués dans chaque technique () ")
    }
    
    
    
  })
  
  output$result_map_click <- renderPlotly({
    
      data_func <- data_selected_village()
      
      data_avec_total <- data_func%>%
        left_join(
          data_secteurs,
          by = c("secteurid" = "id")
        )
      
      if(input$indicateurs == "Proportions de pêcheurs enquêtés par filières halieutiques"){
        data_avec_total <- data_avec_total %>%
          rename(
            `Petit poisson` = Petit_poisson,
            `Gros poisson` = Gros_poisson,
            `Poisson courbine` = Poisson_courbine
          )
      }else if(input$indicateurs == "Proportions de pêcheurs enquêtés par techniques de pêches"){
        data_avec_total <- data_avec_total %>%
          rename(
            `filet maillant` = filet_maillant,
            `filet moustiquaire` = filet_moustiquaire,
            `ligne à la main` = ligne_main
          )
      }
      
      if(is.null(selected_village())){
        
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
              data_avec_total <- data_avec_total %>%
                summarise(
                Chevaquine = sum(Chevaquine, na.rm = TRUE),
                Crabe = sum(Crabe, na.rm = TRUE),
                Crevette = sum(Crevette, na.rm = TRUE),
                `Gros poisson` = sum(`Gros poisson`, na.rm = TRUE),
                `Petit poisson` = sum(`Petit poisson`, na.rm = TRUE),
                `Poisson courbine` = sum(`Poisson courbine`, na.rm = TRUE),
                nb_pecheur = sum(nb_pecheur_secteur, na.rm = TRUE)
            )
          )
        }else if(input$indicateurs == "Proportions de pêcheurs enquêtés par techniques de pêches"){
          data_avec_total <- data_avec_total %>%
            summarise(
              data_avec_total <- data_avec_total %>%
                summarise(
                  `filet maillant` = sum(`filet maillant`, na.rm = TRUE),
                  `filet moustiquaire` = sum(`filet moustiquaire`, na.rm = TRUE),
                  `ligne à la main` = sum(`ligne à la main`, na.rm = TRUE),
                  `palangre` = sum(`palangre`, na.rm = TRUE),
                  `balance` = sum(`balance`, na.rm = TRUE),
                  `crochet` = sum(`crochet`, na.rm = TRUE),
                  `senne` = sum(`senne`, na.rm = TRUE),
                  `raquette` = sum(`raquette`, na.rm = TRUE),
                  `casier` = sum(`casier`, na.rm = TRUE),
                  nb_pecheur = sum(nb_pecheur_secteur, na.rm = TRUE)
                )
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
          select(Chevaquine, Crabe, Crevette, `Gros poisson`,`Petit poisson`, `Poisson courbine`) %>%
          pivot_longer(everything(), names_to = "categorie", values_to = "nombre")
      }else if(input$indicateurs == "Proportions de pêcheurs enquêtés par techniques de pêches"){
        df_long <- data_avec_total %>%
          select(`filet maillant`, `filet moustiquaire`, `ligne à la main`, palangre,balance,crochet,senne,raquette,casier) %>%
          pivot_longer(everything(), names_to = "categorie", values_to = "nombre")
      }
    
      View(data_avec_total)
      
      if(input$indicateurs == "Démographie de la communauté des pêcheurs"){
    
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
      }else{
        if(input$indicateurs == "Proportions de pêcheurs enquêtés par filières halieutiques"){
          xaxis<-"Filières"
          color<-'#2596be'
        }
        else if(input$indicateurs == "Proportions de pêcheurs enquêtés par techniques de pêches"){
          xaxis<-"Techniques"
          color<-'#873e23'
        }
        total <- data_avec_total$nb_pecheur
        df_long <- df_long %>%
          mutate(total_pecheur = total)
      plot_ly(
        df_long,
        x = ~categorie,      # catégories sur l’axe X
        y = ~nombre/total_pecheur*100,         # valeurs sur l’axe Y
        type = 'bar',
        text = ~paste0(nombre, " pêcheur(s)"),             # texte à afficher
        textposition = 'outside',
        marker = list(color = color)
        # type de graphique
      ) %>%
        layout(
          xaxis = list(title = xaxis),
          yaxis = list(title = "Proportion des pêcheurs (%)"),
          paper_bgcolor = "rgba(240, 248, 255, 1)",  # fond global
          plot_bgcolor = "rgba(255, 255, 255, 0)",   # fond du graphique
          margin = list(
            l = 40,
            r = 40,
            b = 60,
            t = 80,
            pad = 20
          )
        )
      }
    
  })
  
  
  output$map_placeholder <- renderLeaflet({
    leaflet(shape_belon) %>%
      addTiles(group = "Vue Plan") %>% 
      addTiles(
        urlTemplate = "https://tiles.stadiamaps.com/tiles/alidade_satellite/{z}/{x}/{y}.jpg",
        attribution = '&copy; <a href="https://stadiamaps.com/" target="_blank">Stadia Maps</a> &copy; <a href="https://openstreetmap.org/copyright" target="_blank">OpenStreetMap</a>',
        options = tileOptions(minZoom = 0, maxZoom = 20),
        group = "Vue Satellite"
      ) %>%
      addTiles(
        urlTemplate = "https://tiles.stadiamaps.com/tiles/alidade_smooth_dark/{z}/{x}/{y}{r}.png",
        attribution = '&copy; <a href="https://stadiamaps.com/" target="_blank">Stadia Maps</a> &copy; <a href="https://openstreetmap.org/copyright" target="_blank">OpenStreetMap</a>',
        options = tileOptions(minZoom = 0, maxZoom = 20),
        group = "Vue sur Fond Noir"
      ) %>%
      addTiles(
        urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
        attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community',
        options = tileOptions(minZoom = 0, maxZoom = 20),
        group = "Arcgis Online"
      ) %>%
      addLayersControl(
        baseGroups = c("Vue Plan", "Vue Satellite","Vue sur Fond Noir","Arcgis Online"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView( lng = 44.2009372, lat = -19.7045099, zoom = 10 )%>%
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
        #popup = ~paste0(VILLAGE, "<br>Nombre pêcheurs : ", nb_pecheur_secteur),
        label = ~paste0(VILLAGE),           # s'affiche au survol
        labelOptions = labelOptions(
          noHide = FALSE,          # FALSE = n’apparaît qu’au survol
          direction = "right",
          textsize = "14px",
          offset = c(0,0)
        )# remplace NOM_COLONNE par le champ qui contient le nom du village
      )
  })
  
  # ===== NETTOYAGE =====
  onStop(function() {
    if (DBI::dbIsValid(pool)) {
      DBI::dbDisconnect(pool)
    }
  })
}