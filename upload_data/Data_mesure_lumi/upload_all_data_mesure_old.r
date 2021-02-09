# . -------------------------------------------------------------------------- =============
# 0 - Lecture des donn√©es issues des capteurs de mesures ====
# . -------------------------------------------------------------------------- =============

# Le script a pour objectif de lire les donn√©es fournirs par les capteurs de mesures et de les enregistrer dans la BDD


# . -------------------------------------------------------------------------- =============
# 1 - Librairie ====
# . -------------------------------------------------------------------------- =============

library(sf)
library(RPostgreSQL) 
library(mapview)
library(readr)
library(readxl)
library(stringr)
library(lubridate)
library(tidyverse)
library(dplyr)


# . -------------------------------------------------------------------------- =============
# 2 - Connexion BDD postGIS ====
# . -------------------------------------------------------------------------- =============

## Supressions de toutes les connexions pr?c?dentes
lapply(dbListConnections(drv = dbDriver("PostgreSQL")),
       function(x) {dbDisconnect(conn = x)})

# type de connexion PostgreSQL
drv <- dbDriver("PostgreSQL")

# Creation de la connexion
source("C:/Users/fa101525/Desktop/GitHub/connect_bdd.R")

# . -------------------------------------------------------------------------- =============
# 3 - fonction lecture fichier/ecriture BDD ====
# . -------------------------------------------------------------------------- =============


lecture_dossier_capteur <- function(dossier) {
  #  dossier = paste(dossier_all_capteur,list_doc_capteur[1],sep = "/")
  # dossier
  # 
  ### DONNEES CAPTEUR DE MESURE  

  dossier_mesure_capteur =  dossier# dossier contenant l'ensemble des fichiers .DTA
  list_fichier_capteur <- dir(path = dossier_mesure_capteur) #liste l'ensemble des fichiers 
  length(list_fichier_capteur) # indique le nombre totale de fichier 
  
  setwd(dossier_mesure_capteur) 
  
  for( i in 1:length(list_fichier_capteur)){ # lecture de chaque fichier .DTA dans le tableau 
    # i = 1
    
    print(paste("Lecture des fichier :", round((i/length(list_fichier_capteur))*100,2) ,"%"))
    extention_fichier = str_sub(list_fichier_capteur[i], start = str_length(list_fichier_capteur[i])-3,end = str_length(list_fichier_capteur[i]) )
    list_fichier_capteur[i]
    
    if(extention_fichier == ".DTA"){ # test extension fichier
      if(ncol(read_delim(list_fichier_capteur[i],"$", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)) > 1){ #test format fichier
      
      site <- str_sub(dossier, start =  str_locate(dossier, "site")[1],end = str_length(dossier) ) #recuperation du site (sur le nom de dossier)
      print(paste("Site :",site)) # affichage du site
      
      # lecture du fichier .DTA
      lecture_fichier <- read_delim(list_fichier_capteur[i],"$", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE) %>%
        t() %>%
        data.frame() %>%
        drop_na()
      names(lecture_fichier) <- c("full_info") #rename nom de la colonne contenant l'ensemble des info
      
      donnee_capteur <- data.frame() # preparation d'un tableau vierge
  
      for(j in 1:nrow(lecture_fichier)){ #sÈparation des information pour chaque ligne du fichier DTA
        out <- read.table(text = lecture_fichier[j,1], sep=";", fill = TRUE, 
                          header = FALSE, stringsAsFactors = FALSE) #les donnÈes sont sÈparÈ par un ; dans la 1ere colonne
        out <- out[,c(-1,-24)] # supression de la premiere et derniere colonne qui sont vide de base 
        # renommer l'ensemble des colones
        names(out) <- c("date","time","on_time",
                        "sumredlightcount","sumgreenlightcount","sumbluelightcount","sumclearlightcount","magvred",
                        "minredfreq","sumredfreq","maxredfreq",
                        "mingreenfreq","sumgreenfreq","maxgreenfreq",
                        "minbluefreq", "sumbluefreq","maxbluefreq",
                        "minclearfreq", "sumclearfreq", "maxclearfreq",
                        "nlightcount", "totalintegrationtime" )
        
        # ajout d'information dans le tableau
        out <- out %>%
          mutate(
            site_mesure = site, #numero du site
            fichier = list_fichier_capteur[i], #fichier d'extraction
            nsb = -1.153*log(sumclearlightcount)+24.203 # La formule pour obtenir le NSB a partir des donnees
          )
        
        # ajout de chaque ligne de lecture dans un tableau du fichier
        donnee_capteur <- donnee_capteur %>%
          rbind(out)
      }
      
      # enregistrement dans une BDD
      if (dbExistsTable(DB_pol_lum, "donnee_capteur")){ #si elle existe alors : 
        st_write(obj = donnee_capteur, dsn = DB_pol_lum, layer = "donnee_capteur", append = TRUE)
      } else{ #si elle existe pas alors :
        requete_fichier_recep ="CREATE TABLE donnee_capteur (
                        date text,time text,on_time integer,
                        sumredlightcount bigint,sumgreenlightcount bigint,sumbluelightcount bigint, sumclearlightcount bigint,magvred double precision, 
                        minredfreq double precision,sumredfreq double precision,maxredfreq double precision,
                        mingreenfreq double precision,sumgreenfreq double precision,maxgreenfreq double precision,
                        minbluefreq double precision, sumbluefreq double precision,maxbluefreq double precision,
                        minclearfreq double precision, sumclearfreq double precision, maxclearfreq double precision,
                        nlightcount bigint, totalintegrationtime bigint,
                        site_mesure text,  fichier text, nsb double precision);"
        dbExecute(DB_pol_lum, requete_fichier_recep)
        
        print(paste("CrÈation table donnee_capteur fait"))
        st_write(obj = donnee_capteur, dsn = DB_pol_lum, layer = "donnee_capteur", append = TRUE)
      }

    }
    
    
    }
  }
}

# . -------------------------------------------------------------------------- =============
# 4 - lancer le traitement des donnees  ====
# . -------------------------------------------------------------------------- =============

# Lecture des donnees DTA des differents capteurs 

dossier_all_capteur =  "C:/Users/fa101525/Desktop/Projet_Pollux/DATA_MESURE_1/data capteurs tot"# dossier contenant l'ensemble des fichiers 
list_doc_capteur <- dir(path = dossier_all_capteur) #liste l'ensemble des fichiers 
length(list_doc_capteur) # indique le nombre totale de fichier 

list_doc_capteur[3]
for( i in 1:length(list_doc_capteur)){ # lecture de chaque fichier DTA dans le tableau  length(list_doc_capteur)
  
    print(paste("ECRITURE DU DOSSIER:", list_doc_capteur[i], " i = ", i))
    lecture_dossier_capteur(paste(dossier_all_capteur,list_doc_capteur[i],sep = "/"))

}


# lecture du fichier des references geographiques des capteurs 

station_capteur <- read_excel("C:/Users/fa101525/Desktop/Projet_Pollux/DATA_MESURE_1/cordonnees gps oct2017_V2.xls") %>%
  mutate(site_mesure_id = paste("site",capteur_V2)) %>%
  st_as_sf(coords = c("X Lambert93 [m]", "Y Lambert93 [m]"), crs = 2154) 

station_capteur <- station_capteur[,c(15,7,6,4,14,16)]
names(station_capteur) <- c("site_mesure_id", "commune","lieu_dit","date", "altitude", "geometry" )

if (dbExistsTable(DB_pol_lum, "station_capteur")){ #si elle existe alors : 
  dbRemoveTable(DB_pol_lum, "station_capteur") # on la supprime
  st_write(obj = station_capteur, dsn = DB_pol_lum, layer = "station_capteur") # on reecrit
} else{ #si elle existe pas alors :
  st_write(obj = station_capteur, dsn = DB_pol_lum, layer = "station_capteur")
}


