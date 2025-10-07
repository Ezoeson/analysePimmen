# global.R
# Ce fichier contient les chargements de bibliothèques, 
# la connexion à la base de données et les variables globales

# Chargement des bibliothèques
library(shiny)
library(ggplot2)
library(dplyr)
library(DBI)
library(RPostgreSQL)
library(forcats)
library(pool)
library(tidyr)

# Configuration de la connexion à la base de données
pool <- dbPool(
  drv = RPostgreSQL::PostgreSQL(),
  dbname = "crabe_socio_eco",
  host = "vps-a8d8821c.vps.ovh.net",
  port = 5433,
  user = "ezoeson",
  password = "Ezoeson123456"
)

# Fonctions utilitaires (si nécessaire)
# Exemple : fonction pour créer un graphique vide
create_empty_plot <- function(message = "Aucune donnée disponible") {
  ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = message, size = 6) +
    theme_void()
}

# Constantes
COULEURS_GENRE <- c("MASCULIN" = "#3498db", "FÉMININ" = "#e74c3c", "Non spécifié" = "#95a5a6")