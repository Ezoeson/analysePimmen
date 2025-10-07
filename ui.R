ui <- bootstrapPage(
  #shinythemes::themeSelector(),
  navbarPage(
    theme = shinytheme("flatly"),
    collapsible = TRUE,
    title = div(
      img(src = "https://via.placeholder.com/30x30/007bff/ffffff?text=P", 
          height = "30", 
          style = "margin-right:10px;"),
      "PIMMEN - Visualisation des données"
    ),
    id = "main_nav",
    tabPanel("Cartographie", icon = shiny::icon("map"),
       div(class="outer",
        tags$head(includeCSS("./styles.css")),
        leafletOutput("map_placeholder", width="100%", height="100%"),
        absolutePanel(id = "controls", class = "panel panel-primary",
          top = 80, left = 50, width = '30%', fixed=TRUE,
          div(class = "main-content",
              h3("Cartographie: Démographie de la communauté de pêcheurs"),
              div(class = "info-box",
                  p(""),
                  p("Source: PIMMEN")
              )
          )
        ),
        absolutePanel(id = "controls", class = "panel panel-primary",
                      style = " border: none;box-shadow: none;
    transition: none;
    pointer-events: auto;",
                      class = "panel panel-primary",
                      top = 220, left = 50, width = '30%', fixed=TRUE,
                      div(class = "main-content",
                          h3(textOutput("text_village_selected")),
                          shinycssloaders::withSpinner(plotlyOutput("result_map_click")),
                          p(textOutput("legende"))
                      )
        ),
        absolutePanel(id = "controls", class = "panel panel-primary",
          bottom = 10, left = 50, width = '95%', fixed=TRUE,
          h4("Chiffre clé du projet PIMMEN"),
          fluidRow(
            column(3, 
                   div(class = "plot-container",
                       h5("Total pêcheurs"),
                       textOutput("total_pecheurs", container = h3)
                   )
            ),
            column(3,
                   div(class = "plot-container",
                       h5("Secteurs couverts"),
                       textOutput("total_secteurs", container = h3)
                   )
            ),
            column(3,
                   div(class = "plot-container",
                       h5("Espèces pêchées"),
                       textOutput("total_especes", container = h3)
                   )
            ),
            column(3,
                   div(class = "plot-container",
                       h5("Enquêtes réalisées"),
                       textOutput("total_enquetes", container = h3)
                   )
            )
          )
        ),
        
      )
    )
  )
)