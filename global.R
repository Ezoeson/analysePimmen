# Chargement des bibliothèques
library(shiny)
library(ggplot2)
library(dplyr)
library(DBI)
library(RPostgres)
library(forcats)
library(pool)
library(tidyr)
library(sf)
library(leaflet)
library(readr)
library(shinycssloaders)
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
library(shinyBS)
library(plotly)

# Configuration de la connexion à la base de données
con <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "crabe_socio_eco",
  host = "vps-a8d8821c.vps.ovh.net",
  port = 5433,
  user = "ezoeson",
  password = "Ezoeson123456"
)



linebreaks <- function(n){HTML(strrep(br(), n))}

# Fonctions utilitaires (si nécessaire)
# Exemple : fonction pour créer un graphique vide
create_empty_plot <- function(message = "Aucune donnée disponible") {
  ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = message, size = 6) +
    theme_void()
}

# Constantes
COULEURS_GENRE <- c("MASCULIN" = "#3498db", "FÉMININ" = "#e74c3c", "Non spécifié" = "#95a5a6")
