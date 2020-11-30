# . -------------------------------------------------------------------------- =============
# 1 - Lecture des données Météo France ====
# . -------------------------------------------------------------------------- =============

# Le script a pour objectif de convertir les données météo france disponible en CSV dans une BDD spatiale. 

# Nous avons deux type de données : 
# - Les stations de mesures géoréférencer : https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/postesSynop.csv
# - Les mesures des stations : https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=90&id_rubrique=32

# le prochain objectif et de permettre de le téléchargement des fichier CSV de manière automatique 

# . -------------------------------------------------------------------------- =============
# 1 - Librairie ====
# . -------------------------------------------------------------------------- =============

library(sf)
library(RPostgreSQL) 
library(mapview)
library(raster)
library(rasterVis)
library(viridis)

library(stringr)
library(lubridate)
library(ggplot2)
library(tidyverse)


# . -------------------------------------------------------------------------- =============
# 2 - Connexion BDD postGIS ====
# . -------------------------------------------------------------------------- =============

## Supressions de toutes les connexions pr?c?dentes
lapply(dbListConnections(drv = dbDriver("PostgreSQL")),
       function(x) {dbDisconnect(conn = x)})

# type de connexion PostgreSQL
drv <- dbDriver("PostgreSQL")

# Cr?ation de la connexion
DB_pol_lum <- dbConnect(RPostgres::Postgres(),  dbname = "Pollux_2",
                        host = "localhost", port = 5432, # attention 5432 par d?faut
                        user = "postgres", password = "XXXXXXXX",
                        options="-c search_path=meteo") # idem pour use

# . -------------------------------------------------------------------------- =============
# 3 - Traitements des fichiers ====
# . -------------------------------------------------------------------------- =============


### DONNEES STATION DE MESURE  

#lecture fichier CSV
station_mesure <- read.csv("C:/Users/fa101525/Desktop/Projet_Pollux/DATA_METEO/postesSynop.csv", sep=";") %>% #lecture CSV
                st_as_sf(coords = c( "Longitude","Latitude"), crs = 4326) %>% #création geom à partir de X Y
                st_transform(crs = st_crs(2154)) #tranformation de la projection en L93 

mapview(station_mesure)  # verification du resultat 

#enregistrement dans la BDD
st_write(station_mesure, DB_pol_lum)



### DONNEES OMM

#lecture fichier CSV
dossier_mesure_OMM = "C:/Users/fa101525/Desktop/Projet_Pollux/DATA_METEO/synop" # dossier contenant l'ensemble des fichiers 
list_fichier_OMM <- dir(path = dossier_mesure_OMM) #liste l'ensemble des fichiers 
length(list_fichier_OMM) # indique le nombre totale de fichier 

setwd(dossier_mesure_OMM) 
donnee_omm <- data.frame() # preparation d'un tableau 

for( i in 1:length(list_fichier_OMM)){ # lecture de chaque fichier CSV dans le tableau 
  donnee_omm <- donnee_omm %>%
    rbind(read.csv(list_fichier_OMM[i], sep=";",na.strings="mq"))
}

donnee_omm <- donnee_omm %>% # remise en forme de la date 
  mutate( date = paste(
      str_sub(date, start = 1, end = 4),"-",   ## extraction année
      str_sub(date, start = 5, end = 6),"-",   ## extraction mois
      str_sub(date, start = 7, end = 8)," ",   ## extraction jour
      str_sub(date, start = 9, end = 10),":",  ## extraction heure
      str_sub(date, start = 11, end = 12),":", ## extraction minutes 
      str_sub(date, start = 13, end = 14)," UTC",  sep="")) ## extraction seconde

#enregistrement dans la BDD
st_write(donnee_omm, DB_pol_lum)
