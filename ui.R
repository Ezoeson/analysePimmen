ui <- bootstrapPage(
  #shinythemes::themeSelector(),
  navbarPage(
    theme = shinytheme("flatly"),
    collapsible = TRUE,
    title = div(
      #actionButton("go_site", "Interface de saisie"),
      img(src = "https://www.transparentpng.com/thumb/database/v8s5DT-world-database-picture.png", 
          height = "30", 
          style = "margin-right:10px;"),
      "PIMMEN - Visualisation des données",
    ),
    id = "main_nav",
    tabPanel("Cartographie", icon = shiny::icon("map"),
       div(class="outer",
        tags$head(includeCSS("./styles.css")),
        leafletOutput("map_placeholder", width="100%", height="100%"),
        absolutePanel(id = "controls", class = "panel panel-primary",
                      style = " border: none;box-shadow: none;
    transition: none;
    pointer-events: auto;",
                      class = "panel panel-primary",
                      top = 150, left = 50, width = '50%', fixed=TRUE,draggable = TRUE,
                      div(class = "main-content",
                          h3(textOutput("indicateurs")),
                          h4(textOutput("text_village_selected")),
                          shinycssloaders::withSpinner(
                            plotlyOutput("result_map_click", height = "600px"),
                            type = 1,       # (optionnel) type de spinner
                            color = "#2596be",
                            color.background = "white"
                          ),
                          p(textOutput("legende"))
                      )
        ),
        absolutePanel(id = "controls", class = "panel panel-primary",
          top = 80, right = 50, width = '30%', fixed=TRUE,
          fluidRow(
            column(12, linebreaks(1),
                   div(class = "parametres-container",
                       shinyWidgets::pickerInput("indicateurs","Indicateurs à visualiser",choices = c("Démographie de la communauté des pêcheurs","Proportions de pêcheurs enquêtés par filières halieutiques"),multiple = FALSE,selected = "Démographie de la communauté des pêcheurs",width = '100%')
                   )
            )
          )
        ),
        
      )
    )
  )
)