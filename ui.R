# ui.R
# Interface utilisateur de l'application Shiny

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
    
    # ===== ONGLET TABLEAU DE BORD =====
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
    
    # ===== ONGLET DÉMOGRAPHIE =====
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
    
    # ===== ONGLET PRATIQUES DE PÊCHE =====
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
    
    # ===== ONGLET SAISONNALITÉ =====
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
  
  # ===== SIDEBAR FIXE =====
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