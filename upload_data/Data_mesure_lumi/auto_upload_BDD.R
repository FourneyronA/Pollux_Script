# . -------------------------------------------------------------------------- =============
# 0 - Lecture des donn√©es issues des capteurs de mesures ====
# . -------------------------------------------------------------------------- =============

# Le script a pour objectif de lire les donnees fournirs par les capteurs de mesures Ninox et de les enregistrer dans la BDD


# . -------------------------------------------------------------------------- =============
# 1 - Librairie ====
# . -------------------------------------------------------------------------- =============

pkgs <-  c("dplyr","tidyverse","lubridate","stringr", "RPostgreSQL", "raster","sf","mapview", "readr", "readxl") # "mapview", "readr", "readxl"

if (length(setdiff(pkgs, rownames(installed.packages()))) > 0) {
  # installation des packages 
  install.packages(setdiff(pkgs, rownames(installed.packages())))  
} 
# chargement des packages 
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)


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
  
  dossier_all_capteur <- "C:/Users/fa101525/Desktop/Projet_Pollux/DATA_MESURE_Ninox"
  list_doc_capteur <- dir(path = dossier_all_capteur) 
  dossier = paste(dossier_all_capteur,list_doc_capteur[4],"",sep = "/")

  ### DONNEES CAPTEUR DE MESURE  
  dossier_mesure_capteur =  dossier# dossier contenant l'ensemble des fichiers .DTA
  list_fichier_capteur <- dir(path = dossier_mesure_capteur) #liste l'ensemble des fichiers 
  length(list_fichier_capteur) # indique le nombre totale de fichier 
  
  setwd(dossier_mesure_capteur) 

  ninox_location <- read.csv(list_fichier_capteur[1])
  ninox_model <- read.csv(list_fichier_capteur[3])
  ninox_sqm <- read.csv(list_fichier_capteur[4])
  
  ninox <- ninox_location %>%
    cbind(ninox_model) %>%
  cbind(ninox_sqm)
  colnames(ninox_model)[1]
  

  ninox_data <- read.csv(list_fichier_capteur[2])

  
  if (!dbExistsTable(DB_pol_lum, "donnee_omm")){
    requete_fichier_donnee_omm ="CREATE TABLE donnee_omm (numer_sta integer,date double precision,date_ok text,time_ok text,pmer integer,tend integer,cod_tend integer,dd integer,ff double precision,
                                                            t double precision,td double precision,u integer,vv integer,ww integer,w1 integer,w2 integer,n integer,nbas integer,hbas integer,
                                                            cl integer,cm integer,ch integer,pres integer,niv_bar integer,geop integer,tend24 integer,tminsol double precision,raf10 double precision,
                                                            etat_sol double precision,ht_neige double precision,ssfrai double precision,perssfrai integer,nnuage1 integer,ctype1 integer,hnuage1 integer,
                                                            nnuage2 integer,ctype2 integer,hnuage2 integer,nnuage3 integer,ctype3 integer,hnuage3 integer,nnuage4 integer,ctype4 integer,hnuage4 integer);"
    
    dbExecute(DB_pol_lum, requete_fichier_donnee_omm)
    print(paste("Creation de la Base de donnee " ))
  }
  
  requete_sql <- paste("SELECT count(*) FROM donnee_omm 
                        WHERE date_ok = '",date,"'
                        AND time_ok = '",horaire,":00:00'",sep ="")
  Donnee_BDD <- dbGetQuery(DB_pol_lum, requete_sql) 
  if (Donnee_BDD == 0){ # verification si la donnee existe deja dans la BDD
    
  }


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

# . -------------------------------------------------------------------------- =============
# 4 - lancer le traitement des donnees  ====
# . -------------------------------------------------------------------------- =============

# Lecture des donnees DTA des differents capteurs 

dossier_all_capteur =  "C:/Users/fa101525/Desktop/Projet_Pollux/DATA_MESURE_Ninox"# dossier contenant l'ensemble des fichiers 
list_doc_capteur <- dir(path = dossier_all_capteur) #liste l'ensemble des fichiers 
length(list_doc_capteur) # indique le nombre totale de fichier 

list_doc_capteur[3]

for( i in 1:length(list_doc_capteur)){ # lecture de chaque fichier DTA dans le tableau  length(list_doc_capteur)
  i = 3
  if(str_detect(list_doc_capteur[i], "ninox")){
    
    print(paste("ECRITURE DU DOSSIER:", list_doc_capteur[i], " i = ", i))
    lecture_dossier_capteur(paste(dossier_all_capteur,list_doc_capteur[i],sep = "/")) 
    
  }
}
