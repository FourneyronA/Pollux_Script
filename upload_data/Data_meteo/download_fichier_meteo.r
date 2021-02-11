
# . -------------------------------------------------------------------------- =============
# 0 - Lecture des donnees Meteo France ====
# . -------------------------------------------------------------------------- =============

# Le script a pour objectif de telecharge les donnees meteo france disponible en CSV/tiff 
# ATTENTION il est imperatif d'adapter les parties suivantes : 
# 2 - Variable et parametre : (emplacement des fichiers)

# Nous avons deux type de donnees : 
# - Les stations de mesures georeferencer : https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/postesSynop.csv
# - Les mesures des stations : https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=90&id_rubrique=32
# - Les deplacements des nuages georeferencer : https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=130&id_rubrique=51

# Si vous souhaitez mettre les donnees telechargee directement dans une base de donnee regarder le script "upload_data_meteo.R"


# . -------------------------------------------------------------------------- =============
# 1 - Librairie ====
# . -------------------------------------------------------------------------- =============

pkgs <-  c("dplyr","tidyverse","lubridate","stringr", "RCurl", "RPostgreSQL", "raster","sf") # "rgdal", "tidyr", "tiff"

if (length(setdiff(pkgs, rownames(installed.packages()))) > 0) {
  # installation des packages 
  install.packages(setdiff(pkgs, rownames(installed.packages())))  
} 
# chargement des packages 
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

# . -------------------------------------------------------------------------- =============
# 2 - / ! \ Variable et parametre / ! \  ====
# . -------------------------------------------------------------------------- =============

# dossier ou serons stocker les fichiers CSV
dossier_enregistrement_csv <- "C:/Users/fa101525/Desktop/Projet_Pollux/DATA_METEO/synop/test_automatik_save"
dossier_enregistrement_geotiff <- "C:/Users/fa101525/Desktop/Projet_Pollux/DATA_METEO/geotiff_meteofrance/"

# enregistrement du TOKEN pour requeter les donnees de meteo france 
source("C:/Users/fa101525/Desktop/GitHub/token_meteofrance.R")

# Date de debut et fin a telecharger (en fonction de la date du jour) 
# Les donnes ne sont diponibles que pendant un certain moment, il faut donc relancer le script tout les 4 jours maximum
# Le script peut etre lancer tout les jours, il verifie si les donnees sont presente ou non avant de lancer le telechargement

date_debut = Sys.Date() %m-% days(4) # Debut 4 jours avant aujourd'hui
date_fin = date_debut %m+% days(3)   # Fin 1 jours avant aujourd'hui


# . -------------------------------------------------------------------------- =============
# 3 - fonction lecture API/ecriture BDD ====
# . -------------------------------------------------------------------------- =============

# les fonctions qui permettent d'interroger le site meteo france 
# conversion du format des donnees puis enregistrement sur la base de donnees

# recuperation des mesures dans les stations (temperature, vent, nuage, neige, ...) 
Requete_meteo_france_omm <- function(date, horaire) {
  
  annee <-  as.numeric(year(date))
  mois <- sprintf("%02d", as.numeric(month(date)))
  jours <- sprintf("%02d", as.numeric(day(date)))
  
  setwd(file.path(dossier_enregistrement_csv))
  file_name <- paste(annee,mois,jours, sep = "_")
  file_omm <- paste0(file_name, "/donnee_omm",  sep = "")

  if(!dir.exists(file_name)){
    print(paste0("Creation du dossier : ",file_name))
    dir.create(file_name)
  } 
  
  if(!dir.exists(file_omm)){
    print(paste0("Creation du dossier : ",file_omm))
    dir.create(file_omm)
  } 
  
  nom_fichier_csv <- paste0(file_omm,"/","date_",date,"heure_",horaire,".csv")
 
  if (!file.exists(nom_fichier_csv)){ # verification si la donnee existe deja dans la BDD
    
    ligne_link <- paste0("https://donneespubliques.meteofrance.fr/?fond=donnee_libre&prefixe=Txt%2FSynop%2Fsynop&extension=csv&date=",date,"&reseau=",horaire)%>%
      getURL() %>%
      textConnection() %>%
      read.csv() %>%
      filter(str_detect(.[,1], "cliquez sur le lien suivant"))
    
    debut_add <- str_locate(ligne_link, "donnees_libres")[1]
    fin_add <- str_locate(ligne_link, ".csv")[2]
    link <- str_sub(ligne_link, start = debut_add , end = fin_add)
    link2 <- str_remove_all(link, "-")
    url <- paste("https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/synop.",annee,mois,jours,horaire,".csv",sep ="")
    #print(url)
  
    download.file(url,destfile= nom_fichier_csv, mode = 'wb',quiet = TRUE)
    Sys.sleep(1)
    fichier_2 <- read.csv(nom_fichier_csv, sep=";",na.strings="mq")
    
    if (ncol(fichier_2) < 2) {
      print(paste("DONNEES NON DISPONIBLE AU : ",date," à ",horaire,"h" ))
      file.remove(nom_fichier_csv)
    } else {
      print(paste("TELECHARGEMENT REUSSIE :", nom_fichier_csv ))
    }
  } else{
    print(paste("FICHIER DEJA PRESENT" ))
  }
}

# recuperation des geometry des nuages (avec indication des octas)
# Var save_on_file 3 possibilite : si = 1 save dans BDD, si = 2 save dans fichier SHP, si = 3 save dans BDD et SHP
Requete_meteo_france_nuage <- function(date, emprise, type_donnee) { 
    # date <-  Sys.Date()%m-% days(3)
  
  annee <-  as.numeric(year(date))
  mois <- sprintf("%02d", as.numeric(month(date)))
  jours <- sprintf("%02d", as.numeric(day(date)))
  
  pos_stop <- str_locate_all(type_donnee, "_")[[1]][2]
  type_name <- str_sub(type_donnee, 0, pos_stop)
  
  setwd(file.path(dossier_enregistrement_csv))
  file_name <- paste(annee,mois,jours, sep = "_")
  file_geotiff <- paste0(file_name, "/",type_name,"donnee_geotiff_", sep = "")
  

  if(!dir.exists(file_name)){
    print(paste0("Creation du dossier : ",file_name))
    dir.create(file_name)
  } 
  
  if(!dir.exists(file_geotiff)){
    print(paste0("Creation du dossier : ",file_geotiff))
    dir.create(file_geotiff)
  } 

  date_ref <- paste(year(date),"-",sprintf("%02d", month(date)),"-",sprintf("%02d", day(date)),"T00:00:00Z", sep ="")

  if (length(dir(path = file_geotiff)) < 24){
    
    for(heure in 0:23){ 
       # heure <- 15
       #emprise <- c("44.717940438742204","46.700972665304704","3.115126052012159","7.081190505137159")  
       #emprise <- c("44.449999724439457","45.950000000000003","3.649999999999999","6.650000794500522")
       # type_donnee <- "HIGH_CLOUD_COVER__GROUND_OR_WATER_SURFACE___"
      nom_fichier <- paste0(file_geotiff,"/Geotiff_",type_name,"date_ref",year(date),"_",month(date),"_",day(date),"_H",sprintf("%02d", heure),".tiff")
      

      if(!file.exists(nom_fichier)){
        date_fin <- paste(year(date),"-",sprintf("%02d", month(date)),"-",sprintf("%02d", day(date)),"T",sprintf("%02d", heure),":00:00Z", sep ="")
        # print(paste("REF :",date_ref, "_____________  ACTUAL :", date_fin))
        url <- paste("https://geoservices.meteofrance.fr/api/",token,
                     "/MF-NWP-GLOBAL-ARPEGE-01-EUROPE-WCS?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&format=image/tiff&coverageId=",
                     type_donnee, date_ref,"&subset=time(",date_fin,")&",
                     "subset=lat(",emprise[1],",",emprise[2],")&subset=long(",emprise[3],",",emprise[4],")", sep = "")
        
 
        download.file(url,destfile= nom_fichier,method = "wininet", mode = 'wb',quiet = TRUE ) #
        nb <- sample(c(5:15), 1)
        print(paste("GeoTiff enregistrer sous :", nom_fichier, "_________________ alea", nb))
        Sys.sleep(nb)
      }else {
        print(paste("FICHIER",nom_fichier,"DEJA PRESENT" ))
      }
    }
    
  }else {
    print(paste("FICHIER DEJA PRESENT" ))
  }
  
}

# . -------------------------------------------------------------------------- =============
# 4 - lancer le telechargement des donnees dans le dossier et sur BDD  ====
# . -------------------------------------------------------------------------- =============


date <- date_debut

while(date < date_fin){
  
  for(heure in c("00","03","06","09","12","15","18","21")){
    print(paste("Lancement telechargement donnees meteo france du : ",day(date),"/",month(date),"/",year(date)," à ",heure,"h" ))
    Requete_meteo_france_omm(date,heure)
  }
  date <- date %m+% days(1)
}


# . -------------------------------------------------------------------------- =============
# 5 - telechargement des Geotiff des nuages ====
# . -------------------------------------------------------------------------- =============


date <- date_debut

while(date < date_fin){
  emprise <- c("44.449999724439457","45.950000000000003","3.649999999999999","6.650000794500522")
  Requete_meteo_france_nuage(date,emprise, "LOW_CLOUD_COVER__GROUND_OR_WATER_SURFACE___")
  Requete_meteo_france_nuage(date,emprise, "TOTAL_CLOUD_COVER__GROUND_OR_WATER_SURFACE___")
  date <- date %m+% days(1)
}




# Relancer un script en cas d'erreur
if(!is.null(warnings())){
  message <- names(last.warning)
  pos_err <- str_locate(message, "HTTP status was ")[2]
  type_erreur <- str_sub(message, (pos_err+1), str_length(message))
  
  
  if(!is.na(type_erreur)){
    
    print(paste("ATTENTION une Erreur de type : ", type_erreur))
    print("Merci de bien vouloir relancer le script")
    
  }else {
    print(paste("Toutes les donnees sont telecharger dans le fichier :",dossier_enregistrement_csv))
  }
  
}else {
  print(paste("Toutes les donnees sont telecharger dans le fichier :",dossier_enregistrement_csv))
}



