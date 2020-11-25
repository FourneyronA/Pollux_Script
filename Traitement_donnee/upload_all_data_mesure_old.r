# . -------------------------------------------------------------------------- =============
# 0 - Lecture des données issues des capteurs de mesures ====
# . -------------------------------------------------------------------------- =============

# Le script a pour objectif de lire les données fournirs par les capteurs de mesures et de les enregistrer dans la BDD


# . -------------------------------------------------------------------------- =============
# 1 - Librairie ====
# . -------------------------------------------------------------------------- =============

library(sf)
library(RPostgreSQL) 
library(mapview)
library(readr)
library(stringr)
library(lubridate)
library(tidyverse)


# . -------------------------------------------------------------------------- =============
# 2 - Connexion BDD postGIS ====
# . -------------------------------------------------------------------------- =============

## Supressions de toutes les connexions pr?c?dentes
lapply(dbListConnections(drv = dbDriver("PostgreSQL")),
       function(x) {dbDisconnect(conn = x)})

# type de connexion PostgreSQL
drv <- dbDriver("PostgreSQL")

# Creation de la connexion
DB_pol_lum <- dbConnect(RPostgres::Postgres(),  dbname = "Pollux_2",
                        host = "localhost", port = 5432, # attention 5432 par d?faut
                        user = "postgres", password = "XXXXXXXX",
                        options="-c search_path=meteo") # idem pour use


# . -------------------------------------------------------------------------- =============
# 3 - fonction lecture fichier/ecriture BDD ====
# . -------------------------------------------------------------------------- =============


lecture_dossier_capteur <- function(dossier) {
  #dossier = paste(dossier_all_capteur,list_doc_capteur[3],sep = "/")
  
  ### DONNEES CAPTEUR DE MESURE  
  #lecture fichier CSV
  dossier_mesure_capteur =  dossier# dossier contenant l'ensemble des fichiers 
  list_fichier_capteur <- dir(path = dossier_mesure_capteur) #liste l'ensemble des fichiers 
  length(list_fichier_capteur) # indique le nombre totale de fichier 
  
  setwd(dossier_mesure_capteur) 
  
  donnee_capteur <- data.frame() # preparation d'un tableau 
  
  for( i in 1:length(list_fichier_capteur)){ # lecture de chaque fichier CSV dans le tableau 
 #i = 3
    print(paste("Lecture des fichier :", round((i/length(list_fichier_capteur))*100,2) ,"%"))
    extention_fichier = str_sub(list_fichier_capteur[i], start = str_length(list_fichier_capteur[i])-3,end = str_length(list_fichier_capteur[i]) )
    list_fichier_capteur[i]
    if(extention_fichier == ".DTA"){
      if(ncol(read_delim(list_fichier_capteur[i],"$", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)) > 1){
      
      site <- str_sub(dossier, start =  str_locate(dossier, "site")[1],end = str_length(dossier) )
      
      lecture_fichier <- read_delim(list_fichier_capteur[i],"$", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE) %>%
        t() %>%
        data.frame() %>%
        drop_na() %>%
        rename(full_info = ".")
      
      lecture_fichier <- lecture_fichier %>%
        mutate(point_virgule = str_locate_all(full_info, ";"),
               ID_ligne = seq.int(nrow(lecture_fichier))) %>%
        unnest(point_virgule) %>%
        group_by(ID_ligne) %>% 
        mutate(col= paste("index", sep="_",seq_along(ID_ligne))) %>%
        spread(key=col, value=point_virgule) %>%
        mutate(
          site_mesure = site,
          fichier = list_fichier_capteur[i],
          date = str_sub(full_info, start = index_1+1,end = index_2-1 ),      # year/month/day représente la date de la mesure.
          time = str_sub(full_info, start = index_2 +1,end =index_3-1) ,      # hour:min:sec représente l'heure de la mesure.
          on_time = as.numeric(str_sub(full_info, start = index_3+1,end =index_4-1)),   # onTime représente le temps (en secondes) depuis le démarrage de la carte.
          sumRedLightCount = as.numeric(str_sub(full_info, start = index_4 +1,end =index_5-1)),  # sumRedLightCount
          sumGreenLightCount = as.numeric(str_sub(full_info, start = index_5 +1,end =index_6-1)),  # sumGreenLightCount
          sumBlueLightCount = as.numeric(str_sub(full_info, start = index_6 +1, end =index_7-1)),  # sumBlueLightCo
          sumClearLightCount = as.numeric(str_sub(full_info, start = index_7 +1,end =index_8-1)),  # sumClearLightCount représentent la totalité des "ticks/impulsions" reçus pendant la période d’intégration principale (30s) pour chacune des composante
          magVRed = as.double(str_sub(full_info, start = index_8 +1,end =index_9-1)),  # magVRed est la magnitude visuelle pour la composante rouge.
          minRedFreq = as.double(str_sub(full_info, start = index_9 +1, end =index_10-1)),   # minRedFreq
          sumRedFreq = as.double(str_sub(full_info, start = index_10 +1,end =index_11-1)),  # minGreenFreq
          maxRedFreq = as.double(str_sub(full_info, start = index_11 +1,end =index_12-1)),   # minBlueFre
          minGreenFreq = as.double(str_sub(full_info, start = index_12+1,end =index_13-1)),  # minClearFreq représentent la fréquence minimale mesurée pendant la période d’intégration pour chacune des composantes.
          sumGreenFreq = as.double(str_sub(full_info, start = index_13 +1,end =index_14-1)),   # sumRedFreq
          maxGreenFreq = as.double(str_sub(full_info, start = index_14 +1, end =index_15-1)),   # sumGreenFreq
          minBlueFreq = as.double(str_sub(full_info, start = index_15 +1,end =index_16-1)),   # sumBlueFre
          sumBlueFreq = as.double(str_sub(full_info, start = index_16 +1,end =index_17-1)),  # sumClearFreq représentent la moyenne des  fréquences (impulsion/s =Hz) pour la période d’intégration principale
          maxBlueFreq = as.double(str_sub(full_info, start = index_17 +1,end =index_18-1)),  # sumClearFreq représentent la moyenne des  fréquences (impulsion/s =Hz) pour la période d’intégration principale
          minClearFreq = as.double(str_sub(full_info, start = index_18 +1,end =index_19-1)),   # sumClearFreq représentent la moyenne des  fréquences (impulsion/s =Hz) pour la période d’intégration principale
          sumClearFreq = as.double(str_sub(full_info, start = index_19 +1, end =index_20-1)),  # sumClearFreq représentent la moyenne des  fréquences (impulsion/s =Hz) pour la période d’intégration principale
          maxClearFreq = as.double(str_sub(full_info, start = index_20 +1,end =index_21-1)),   # sumClearFreq représentent la moyenne des  fréquences (impulsion/s =Hz) pour la période d’intégration principale
          nLightCount = as.double(str_sub(full_info, start = index_21 +1, end =index_22-1)),   # sumClearFreq représentent la moyenne des  fréquences (impulsion/s =Hz) pour la période d’intégration principale
          totalIntegrationTime = as.double(str_sub(full_info, start = index_22 +1,end =index_23-1)),  # sumClearFreq représentent la moyenne des  fréquences (impulsion/s =Hz) pour la période d’intégration principale
          nsb = -1.153*log(sumClearLightCount)+24.203 # La formule pour obtenir le NSB en mag/arcsec² est -1.153*log(sumClearLightCount)+24.203.
        )
      
      lecture_fichier <- lecture_fichier[,-1:-25]
      
      donnee_capteur <- donnee_capteur %>%
        rbind(lecture_fichier)
    }
    
    
    }
  }
  
  if (dbExistsTable(DB_pol_lum, "donnee_capteur")){ #si elle existe alors : 
    st_write(obj = donnee_capteur, dsn = DB_pol_lum, layer = "donnee_capteur", append = TRUE)
  } else{ #si elle existe pas alors :
    st_write(obj = donnee_capteur, dsn = DB_pol_lum, layer = "donnee_capteur")
  }
  
}

# . -------------------------------------------------------------------------- =============
# 4 - lancer le traitement des données  ====
# . -------------------------------------------------------------------------- =============


dossier_all_capteur =  "C:/Users/fa101525/Desktop/Projet_Pollux/DATA_MESURE_1/data capteurs tot"# dossier contenant l'ensemble des fichiers 
list_doc_capteur <- dir(path = dossier_all_capteur) #liste l'ensemble des fichiers 
length(list_doc_capteur) # indique le nombre totale de fichier 


for( i in 1:length(list_doc_capteur)){ # lecture de chaque fichier CSV dans le tableau  length(list_doc_capteur)

    print(paste("ECRITURE DU DOSSIER:", list_doc_capteur[i], " i = ", i))
    lecture_dossier_capteur(paste(dossier_all_capteur,list_doc_capteur[i],sep = "/"))

}
