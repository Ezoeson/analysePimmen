library(shiny)
library(ggplot2)
library(dplyr)
library(DBI)
library(RPostgreSQL) # ou library(RSQLite) selon votre base
library(forcats)
library(pool)

# Configuration de la connexion à la base de données
pool <- dbPool(
  drv = RPostgreSQL::PostgreSQL(), # Adaptez selon votre SGBD
  dbname = "crabe_socio_eco",
  host = "vps-a8d8821c.vps.ovh.net",
  port = 5433,
  user = "ezoeson",
  password = "Ezoeson123456")


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #globalSidebar {
        position: fixed;
        top: 70px;
        left: 0;
        bottom: 0;
        width: 280px;
        padding: 20px;
        background-color: #f8f9fa;
        border-right: 1px solid #dee2e6;
        overflow-y: auto;
        z-index: 1000;
      }
      .main-content {
        margin-left: 300px;
        padding: 20px;
        min-height: calc(100vh - 70px);
      }
      .navbar {
        z-index: 1001;
      }
      .plot-container {
        background: white;
        border-radius: 8px;
        padding: 15px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-bottom: 20px;
      }
      .info-box {
        background: #e9f7ff;
        border-left: 4px solid #007bff;
        padding: 15px;
        margin: 15px 0;
        border-radius: 4px;
      }
      .loading {
        text-align: center;
        color: #6c757d;
        font-style: italic;
      }
    "))
  ),
  
  navbarPage(
    title = div(
      img(src = "https://via.placeholder.com/30x30/007bff/ffffff?text=P", 
          height = "30", 
          style = "margin-right:10px;"),
      "PIMMEN - Visualisation des données"
    ),
    id = "main_nav",
    collapsible = TRUE,
    
    tabPanel(
      "Tableau de bord",
      div(class = "main-content",
          h2("Tableau de bord PIMMEN"),
          div(class = "info-box",
              p("Visualisation et analyse des activités de pêche dans le delta de la Tsiribihina, région Menabe."),
              p("Développé par Ezoeson.")
          ),
          
          # KPI Cards
          fluidRow(
            column(3, 
                   div(class = "plot-container",
                       h4("Total pêcheurs"),
                       textOutput("total_pecheurs", container = h3)
                   )
            ),
            column(3,
                   div(class = "plot-container",
                       h4("Secteurs couverts"),
                       textOutput("total_secteurs", container = h3)
                   )
            ),
            column(3,
                   div(class = "plot-container",
                       h4("Espèces pêchées"),
                       textOutput("total_especes", container = h3)
                   )
            ),
            column(3,
                   div(class = "plot-container",
                       h4("Enquêtes réalisées"),
                       textOutput("total_enquetes", container = h3)
                   )
            )
          ),
          
          # Carte placeholder
          div(class = "plot-container",
              h4("Localisation des secteurs"),
              plotOutput("map_placeholder", height = "400px")
          )
      )
    ),
    
    tabPanel(
      "Démographie",
      div(class = "main-content",
          h2("Démographie des pêcheurs par secteur"),
          p("Cette section présente le nombre total de pêcheurs par secteur et la proportion hommes/femmes."),
          
          # Graphiques démographiques
          fluidRow(
            column(8,
                   div(class = "plot-container",
                       h4("Répartition par secteur et genre"),
                       plotOutput("bar_demographie", height = "400px")
                   )
            ),
            column(4,
                   div(class = "plot-container",
                       h4("Distribution par âge"),
                       plotOutput("hist_age", height = "400px")
                   )
            )
          ),
          
          fluidRow(
            column(6,
                   div(class = "plot-container",
                       h4("Répartition par genre"),
                       plotOutput("pie_genre", height = "300px")
                   )
            ),
            column(6,
                   div(class = "plot-container",
                       h4("Niveau d'éducation"),
                       plotOutput("bar_education", height = "300px")
                   )
            )
          )
      )
    ),
    
    tabPanel(
      "Pratiques de pêche",
      div(class = "main-content",
          h2("Pratiques de pêche par espèce"),
          p("Analyse des pratiques de pêche par espèce cible et saison."),
          
          fluidRow(
            column(8,
                   div(class = "plot-container",
                       h4("Espèces cibles principales"),
                       plotOutput("bar_especes", height = "400px")
                   )
            ),
            column(4,
                   div(class = "plot-container",
                       h4("Top 5 espèces"),
                       plotOutput("pie_top_especes", height = "400px")
                   )
            )
          ),
          
          fluidRow(
            column(6,
                   div(class = "plot-container",
                       h4("Fréquence des sorties par saison"),
                       plotOutput("boxplot_frequence", height = "300px")
                   )
            ),
            column(6,
                   div(class = "plot-container",
                       h4("Captures moyennes (kg)"),
                       plotOutput("boxplot_captures", height = "300px")
                   )
            )
          )
      )
    ),
    
    tabPanel(
      "Saisonnalité",
      div(class = "main-content",
          h2("Analyse saisonnière des activités"),
          p("Comparaison des pratiques entre saison haute et saison basse."),
          
          fluidRow(
            column(6,
                   div(class = "plot-container",
                       h4("Durée des saisons (jours)"),
                       plotOutput("bar_duree_saison", height = "300px")
                   )
            ),
            column(6,
                   div(class = "plot-container",
                       h4("Fréquence hebdomadaire"),
                       plotOutput("bar_frequence", height = "300px")
                   )
            )
          ),
          
          fluidRow(
            column(12,
                   div(class = "plot-container",
                       h4("Comparaison captures saisonnières"),
                       plotOutput("violin_captures", height = "400px")
                   )
            )
          )
      )
    )
  ),
  
  absolutePanel(
    id = "globalSidebar",
    top = 70, left = 0, width = 280, fixed = TRUE,
    div(
      h4("Filtres globaux", style = "color: #2c3e50;"),
      hr(style = "border-color: #bdc3c7;"),
      
      # Sélection des secteurs
      selectInput(
        inputId = "secteurs",
        label = "Secteurs :",
        choices = c("Chargement..." = ""),
        multiple = TRUE,
        selectize = TRUE
      ),
      
      # Sélection des espèces
      selectInput(
        inputId = "especes",
        label = "Espèces :",
        choices = c("Toutes" = "all"),
        multiple = TRUE,
        selectize = TRUE
      ),
      
      # Filtre par genre
      selectInput(
        inputId = "genre",
        label = "Genre :",
        choices = c("Tous" = "all", "MASCULIN" = "MASCULIN", "FÉMININ" = "FÉMININ"),
        selected = "all"
      ),
      
      # Filtre par âge
      sliderInput(
        inputId = "age",
        label = "Tranche d'âge :",
        min = 15,
        max = 80,
        value = c(20, 60),
        step = 5
      ),
      
      actionButton("applyFilter", "Appliquer les filtres", 
                   class = "btn btn-primary w-100 mt-3",
                   icon = icon("filter")),
      
      actionButton("resetFilter", "Réinitialiser", 
                   class = "btn btn-outline-secondary w-100 mt-2",
                   icon = icon("refresh")),
      
      hr(style = "border-color: #bdc3c7;"),
      div(
        h5("Statistiques actuelles"),
        textOutput("stats_filtrees", container = p)
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Chargement des données de référence
  data_secteurs <- reactive({
    dbGetQuery(pool, "SELECT id, nom, nb_pecheur_secteur FROM secteurs ORDER BY nom")
  })
  
  data_especes <- reactive({
    dbGetQuery(pool, "SELECT id, nom FROM especes_peche ORDER BY nom")
  })
  
  # Mise à jour des sélecteurs
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
  
  # Graphiques démographiques
  output$bar_demographie <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = "Aucune donnée disponible", size = 6) +
               theme_void())
    }
    
    data_summary <- data %>%
      group_by(secteur_nom, sexe) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(sexe = ifelse(is.na(sexe), "Non spécifié", sexe))
    
    ggplot(data_summary, aes(x = secteur_nom, y = count, fill = sexe)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("MASCULIN" = "#3498db", "FÉMININ" = "#e74c3c", "Non spécifié" = "#95a5a6")) +
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
      scale_fill_manual(values = c("MASCULIN" = "#3498db", "FÉMININ" = "#e74c3c", "Non spécifié" = "#95a5a6")) +
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
  
  # Graphiques pratiques de pêche
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
  
  # Graphiques saisonnalité
  output$bar_duree_saison <- renderPlot({
    data <- filtered_data()
    
    if(nrow(data) == 0) return(ggplot() + theme_void())
    
    duree_data <- data %>%
      filter(!is.na(espece_nom)) %>%
      group_by(espece_nom) %>%
      summarise(
        duree_haute = mean(dureeSaisonHaute, na.rm = TRUE),
        duree_basse = mean(dureeSaisonBasse, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = c(duree_haute, duree_basse), 
                   names_to = "saison", values_to = "duree")
    
    ggplot(duree_data, aes(x = espece_nom, y = duree, fill = saison)) +
      geom_col(position = "dodge") +
      labs(x = "Espèce", y = "Durée moyenne (jours)", fill = "Saison") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$stats_filtrees <- renderText({
    data <- filtered_data()
    paste("Pêcheurs:", length(unique(data$id)), 
          "| Secteurs:", length(unique(data$secteur_nom)),
          "| Espèces:", length(unique(na.omit(data$espece_nom))))
  })
  
  # Placeholder map
  output$map_placeholder <- renderPlot({
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, 
               label = "Carte des secteurs de pêche\n(Région Menabe - Delta de la Tsiribihina)", 
               size = 6, color = "gray") +
      theme_void() +
      theme(panel.background = element_rect(fill = "lightblue", color = NA))
  })
  
  # Nettoyage de la connexion
  onStop(function() {
    poolClose(pool)
  })
}

shinyApp(ui, server)