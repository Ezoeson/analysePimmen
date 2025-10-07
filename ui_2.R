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
              leafletOutput("map_placeholder", height = "400px")
          )
      )
    )
  )
)