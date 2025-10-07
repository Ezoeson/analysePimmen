# server.R
# Logique serveur de l'application Shiny

server <- function(input, output, session) {
  
  # ===== CHARGEMENT DES DONNÉES DE RÉFÉRENCE =====
  data_secteurs <- reactive({
    dbGetQuery(pool, "SELECT id, nom, nb_pecheur_secteur FROM secteurs ORDER BY nom")
  })
  
  data_especes <- reactive({
    dbGetQuery(pool, "SELECT id, nom FROM especes_peche ORDER BY nom")
  })
  
  # ===== MISE À JOUR DES SÉLECTEURS =====
  observe({
    secteurs <- data_secteurs()
    updateSelectInput(session, "secteurs", 
                      choices = setNames(secteurs$id, secteurs$nom))
  })
  
  observe({
    especes <- data_especes()
    choices <- c("Toutes" = "all")
    species_choices <- setNames(especes$id, especes$nom)
    updateSelectInput(session, "especes", 
                      choices = c(choices, species_choices))
  })
  
  # ===== DONNÉES FILTRÉES =====
  filtered_data <- reactive({
    req(input$secteurs)
    
    query <- "
    SELECT 
      e.id,
      e.nomRepondant,
      e.age,
      e.sexe,
      e.niveauEducation,
      e.estPecheur,
      s.nom as secteur_nom,
      pp.especePecheId,
      esp.nom as espece_nom,
      pp.anneeDebut,
      pp.dureeSaisonHaute,
      pp.dureeSaisonBasse,
      pp.frequenceSortiesHebdoSaisonHaute,
      pp.frequenceSortiesHebdoSaisonBasse,
      pp.capturesMoyennesSaisonHaute,
      pp.capturesMoyennesSaisonBasse,
      pp.classificationActivite
    FROM enquetes e
    LEFT JOIN secteurs s ON e.secteurId = s.id
    LEFT JOIN pratiques_peche pp ON e.id = pp.pecheurId
    LEFT JOIN especes_peche esp ON pp.especePecheId = esp.id
    WHERE e.estPecheur = true
    "
    
    # Filtres dynamiques
    if(length(input$secteurs) > 0) {
      secteurs_list <- paste0("'", paste(input$secteurs, collapse = "','"), "'")
      query <- paste0(query, " AND e.secteurId IN (", secteurs_list, ")")
    }
    
    if(input$genre != "all") {
      query <- paste0(query, " AND e.sexe = '", input$genre, "'")
    }
    
    query <- paste0(query, " AND e.age BETWEEN ", input$age[1], " AND ", input$age[2])
    
    if(!is.null(input$especes) && !"all" %in% input$especes) {
      especes_list <- paste0("'", paste(input$especes, collapse = "','"), "'")
      query <- paste0(query, " AND pp.especePecheId IN (", especes_list, ")")
    }
    
    dbGetQuery(pool, query)
  })
  
  # ===== KPI - INDICATEURS =====
  output$total_pecheurs <- renderText({
    data <- filtered_data()
    length(unique(data$id))
  })
  
  output$total_secteurs <- renderText({
    data <- filtered_data()
    length(unique(data$secteur_nom))
  })
  
  output$total_especes <- renderText({
    data <- filtered_data()
    length(unique(na.omit(data$espece_nom)))
  })
  
  output$total_enquetes <- renderText({
    nrow(filtered_data())
  })
  
  output$stats_filtrees <- renderText({
    data <- filtered_data()
    paste("Pêcheurs:", length(unique(data$id)), 
          "| Secteurs:", length(unique(data$secteur_nom)),
          "| Espèces:", length(unique(na.omit(data$espece_nom))))
  })
  
  # ===== GRAPHIQUES DÉMOGRAPHIQUES =====
  output$bar_demographie <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(create_empty_plot())
    }
    
    data_summary <- data %>%
      group_by(secteur_nom, sexe) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(sexe = ifelse(is.na(sexe), "Non spécifié", sexe))
    
    ggplot(data_summary, aes(x = secteur_nom, y = count, fill = sexe)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = COULEURS_GENRE) +
      labs(x = "Secteur", y = "Nombre de pêcheurs", fill = "Genre") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$hist_age <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) return(ggplot() + theme_void())
    
    ggplot(data, aes(x = age)) +
      geom_histogram(fill = "#2ecc71", alpha = 0.7, bins = 20) +
      labs(x = "Âge", y = "Nombre de pêcheurs", title = "Distribution des âges") +
      theme_minimal()
  })
  
  output$pie_genre <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) return(ggplot() + theme_void())
    
    genre_summary <- data %>%
      count(sexe) %>%
      mutate(sexe = ifelse(is.na(sexe), "Non spécifié", sexe))
    
    ggplot(genre_summary, aes(x = "", y = n, fill = sexe)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = COULEURS_GENRE) +
      theme_void() +
      labs(fill = "Genre")
  })
  
  output$bar_education <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) return(ggplot() + theme_void())
    
    education_summary <- data %>%
      count(niveauEducation) %>%
      mutate(niveauEducation = ifelse(is.na(niveauEducation), "Non spécifié", niveauEducation))
    
    ggplot(education_summary, aes(x = fct_reorder(niveauEducation, n), y = n)) +
      geom_col(fill = "#9b59b6", alpha = 0.7) +
      coord_flip() +
      labs(x = "Niveau d'éducation", y = "Nombre de pêcheurs") +
      theme_minimal()
  })
  
  # ===== GRAPHIQUES PRATIQUES DE PÊCHE =====
  output$bar_especes <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) return(ggplot() + theme_void())
    
    especes_summary <- data %>%
      filter(!is.na(espece_nom)) %>%
      count(espece_nom) %>%
      arrange(desc(n))
    
    ggplot(especes_summary, aes(x = fct_reorder(espece_nom, n), y = n)) +
      geom_col(fill = "#e67e22", alpha = 0.7) +
      coord_flip() +
      labs(x = "Espèce", y = "Nombre de pêcheurs") +
      theme_minimal()
  })
  
  output$pie_top_especes <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) return(ggplot() + theme_void())
    
    top_especes <- data %>%
      filter(!is.na(espece_nom)) %>%
      count(espece_nom) %>%
      arrange(desc(n)) %>%
      head(5)
    
    ggplot(top_especes, aes(x = "", y = n, fill = espece_nom)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Set3") +
      theme_void() +
      labs(fill = "Espèce")
  })
  
  output$boxplot_frequence <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) return(ggplot() + theme_void())
    
    # Préparer les données pour la fréquence
    freq_data <- data %>%
      filter(!is.na(espece_nom)) %>%
      select(espece_nom, frequenceSortiesHebdoSaisonHaute, frequenceSortiesHebdoSaisonBasse) %>%
      pivot_longer(cols = c(frequenceSortiesHebdoSaisonHaute, frequenceSortiesHebdoSaisonBasse),
                   names_to = "saison", values_to = "frequence") %>%
      mutate(saison = ifelse(grepl("Haute", saison), "Haute", "Basse"))
    
    ggplot(freq_data, aes(x = saison, y = frequence, fill = saison)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = c("Haute" = "#e74c3c", "Basse" = "#3498db")) +
      labs(x = "Saison", y = "Fréquence hebdomadaire", fill = "Saison") +
      theme_minimal()
  })
  
  output$boxplot_captures <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) return(ggplot() + theme_void())
    
    # Préparer les données pour les captures
    captures_data <- data %>%
      filter(!is.na(espece_nom)) %>%
      select(espece_nom, capturesMoyennesSaisonHaute, capturesMoyennesSaisonBasse) %>%
      pivot_longer(cols = c(capturesMoyennesSaisonHaute, capturesMoyennesSaisonBasse),
                   names_to = "saison", values_to = "captures") %>%
      mutate(saison = ifelse(grepl("Haute", saison), "Haute", "Basse"))
    
    ggplot(captures_data, aes(x = saison, y = captures, fill = saison)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_manual(values = c("Haute" = "#e74c3c", "Basse" = "#3498db")) +
      labs(x = "Saison", y = "Captures moyennes (kg)", fill = "Saison") +
      theme_minimal()
  })
  
  # ===== GRAPHIQUES SAISONNALITÉ =====
  output$bar_duree_saison <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) return(ggplot() + theme_void())
    
    duree_data <- data %>%
      filter(!is.na(espece_nom)) %>%
      group_by(espece_nom) %>%
      summarise(
        duree_haute = mean(dureeSaisonHaute, na.rm = TRUE),
        duree_basse = mean(dureeSaisonBasse, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      pivot_longer(cols = c(duree_haute, duree_basse), 
                   names_to = "saison", values_to = "duree") %>%
      mutate(saison = ifelse(saison == "duree_haute", "Haute", "Basse"))
    
    ggplot(duree_data, aes(x = espece_nom, y = duree, fill = saison)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("Haute" = "#e74c3c", "Basse" = "#3498db")) +
      labs(x = "Espèce", y = "Durée moyenne (jours)", fill = "Saison") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$bar_frequence <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) return(ggplot() + theme_void())
    
    freq_data <- data %>%
      filter(!is.na(espece_nom)) %>%
      group_by(espece_nom) %>%
      summarise(
        freq_haute = mean(frequenceSortiesHebdoSaisonHaute, na.rm = TRUE),
        freq_basse = mean(frequenceSortiesHebdoSaisonBasse, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      pivot_longer(cols = c(freq_haute, freq_basse), 
                   names_to = "saison", values_to = "frequence") %>%
      mutate(saison = ifelse(saison == "freq_haute", "Haute", "Basse"))
    
    ggplot(freq_data, aes(x = espece_nom, y = frequence, fill = saison)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("Haute" = "#e74c3c", "Basse" = "#3498db")) +
      labs(x = "Espèce", y = "Fréquence hebdomadaire moyenne", fill = "Saison") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$violin_captures <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) return(ggplot() + theme_void())
    
    captures_data <- data %>%
      filter(!is.na(espece_nom)) %>%
      select(espece_nom, capturesMoyennesSaisonHaute, capturesMoyennesSaisonBasse) %>%
      pivot_longer(cols = c(capturesMoyennesSaisonHaute, capturesMoyennesSaisonBasse),
                   names_to = "saison", values_to = "captures") %>%
      mutate(saison = ifelse(grepl("Haute", saison), "Saison Haute", "Saison Basse"))
    
    ggplot(captures_data, aes(x = espece_nom, y = captures, fill = saison)) +
      geom_violin(alpha = 0.7, position = position_dodge(width = 0.9)) +
      geom_boxplot(width = 0.1, position = position_dodge(width = 0.9), alpha = 0.5) +
      scale_fill_manual(values = c("Saison Haute" = "#e74c3c", "Saison Basse" = "#3498db")) +
      labs(x = "Espèce", y = "Captures (kg)", fill = "Saison") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # ===== CARTE PLACEHOLDER =====
  output$map_placeholder <- renderPlot({
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, 
               label = "Carte des secteurs de pêche\n(Région Menabe - Delta de la Tsiribihina)", 
               size = 6, color = "gray") +
      theme_void() +
      theme(panel.background = element_rect(fill = "lightblue", color = NA))
  })
  
  # ===== NETTOYAGE =====
  onStop(function() {
    poolClose(pool)
  })
}