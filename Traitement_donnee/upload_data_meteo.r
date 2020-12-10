
# . -------------------------------------------------------------------------- =============
# 0 - Lecture des donnees Meteo France ====
# . -------------------------------------------------------------------------- =============

# Le script a pour objectif de convertir les donnees meteo france disponible en CSV dans une BDD spatiale. 
# ATTENTION il est imperatif d'adapter les parties suivantes : 
        # 2 - Variable et parametre : (emplacement des fichiers)
        # 3 - Connexion BDD postGIS : (connexion a votre base de donnee)

# Nous avons deux type de donnees : 
# - Les stations de mesures georeferencer : https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/postesSynop.csv
# - Les mesures des stations : https://donneespubliques.meteofrance.fr/efond=produit&id_produit=90&id_rubrique=32
# - Les deplacements des nuages géoreferencer : https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=130&id_rubrique=51

# . -------------------------------------------------------------------------- =============
# 1 - Librairie ====
# . -------------------------------------------------------------------------- =============

library(RPostgreSQL) 
library(RCurl)
library(dplyr) 
library(tidyverse)
library(lubridate) 
library(stringr)
library(sf)
library(raster)


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
# 3 - Connexion BDD postGIS ====
# . -------------------------------------------------------------------------- =============

## Supressions de toutes les connexions precedentes
lapply(dbListConnections(drv = dbDriver("PostgreSQL")),
       function(x) {dbDisconnect(conn = x)})

# type de connexion PostgreSQL
drv <- dbDriver("PostgreSQL")

# Creation de la connexion
# DB_pol_lum <- dbConnect(RPostgres::Postgres(), dbname = "XXXX", host = "XXXX", port = XXXX, user = "XXXX", password = "XXXX")
source("C:/Users/fa101525/Desktop/GitHub/connect_bdd.R")

# . -------------------------------------------------------------------------- =============
# 4 - fonction lecture API/ecriture BDD ====
# . -------------------------------------------------------------------------- =============

# les fonctions qui permettent d'interroger le site meteo france 
# conversion du format des donnees puis enregistrement sur la base de donnees

# recuperation des mesures dans les stations (temperature, vent, nuage, neige, ...) 
Requete_meteo_france_omm <- function(date, horaire) {

  annee <-  as.numeric(year(date))
  mois <- sprintf("%02d", as.numeric(month(date)))
  jours <- sprintf("%02d", as.numeric(day(date)))

  setwd(file.path(dossier_enregistrement_csv))
  
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
    
    nom_fichier <- paste0("date_",date,"heure_",horaire,".csv")
    download.file(url,destfile= nom_fichier, mode = 'wb')
    Sys.sleep(1)
    fichier_2 <- read.csv(nom_fichier, sep=";",na.strings="mq")
    
    if (ncol(fichier_2) < 2) {
 
      print(paste("DONNEES NON DISPONIBLE AU : ",date," à ",horaire,"h" ))
      file.remove(nom_fichier)
    } else {
      list_colum <- colnames(fichier_2) %>%
        unlist() %>%
        paste( collapse = ' ')
      
      donnee_omm <- fichier_2 %>% 
        mutate(nuage1_verif = ifelse(str_detect(list_colum,"nnuage1"),1, 0),
               nuage2_verif = ifelse(str_detect(list_colum,"nnuage2"),1, 0),
               nuage3_verif = ifelse(str_detect(list_colum,"nnuage3"),1, 0),
               nuage4_verif = ifelse(str_detect(list_colum,"nnuage4"),1, 0),
               nnuage1 = ifelse(nuage1_verif == 1,nnuage1,NA ),
               ctype1 = ifelse(nuage1_verif == 1,ctype1,NA ),
               hnuage1 = ifelse(nuage1_verif == 1,hnuage1,NA ),
               nnuage2 = ifelse(nuage2_verif == 1,nnuage2,NA ),
               ctype2 = ifelse(nuage2_verif == 1,ctype2,NA ),
               hnuage2 = ifelse(nuage2_verif == 1,hnuage2,NA ),
               nnuage3 = ifelse(nuage3_verif == 1,nnuage3,NA ),
               ctype3 = ifelse(nuage3_verif == 1,ctype3,NA ),
               hnuage3 = ifelse(nuage3_verif == 1,hnuage3,NA ),
               nnuage4 = ifelse(nuage4_verif == 1,nnuage4,NA ),
               ctype4 = ifelse(nuage4_verif == 1,ctype4,NA ),
               hnuage4 = ifelse(nuage4_verif == 1,hnuage4,NA ) ,
               date_ok = paste(annee,mois,jours, sep = "-"),
               time_ok = paste(horaire,":00:00", sep ="") ) ## extraction seconde
      
      donnee_omm <- donnee_omm[ ,c("numer_sta","date","date_ok","time_ok","pmer","tend","cod_tend","dd","ff","t","td","u","vv","ww","w1","w2","n","nbas","hbas","cl","cm","ch","pres","niv_bar","geop","tend24","tminsol","raf10","etat_sol","ht_neige","ssfrai","perssfrai","nnuage1","ctype1","hnuage1","nnuage2","ctype2","hnuage2","nnuage3","ctype3","hnuage3","nnuage4","ctype4","hnuage4")]
      
      
      st_write(obj = donnee_omm, dsn = DB_pol_lum, layer = "donnee_omm", append = TRUE)
      file.remove(nom_fichier)
      print(paste( nrow(donnee_omm)," Donnees enregistrer dans la BDD" ))
    }
  } else{
    print(paste("Donnees dejà presente dans la BDD" ))
  }
}

# recuperation des geometry des nuages (avec indication des octas)
Requete_meteo_france_nuage <- function(date) { 
  
  #date <-  Sys.Date()%m-% days(3)
  date_ref <- paste(year(date),"-",sprintf("%02d", month(date)),"-",sprintf("%02d", day(date)),"T00:00:00Z", sep ="")
  
  if (!dbExistsTable(DB_pol_lum, "nuage_geom_omm")){
    requete_nuage_omm ="CREATE TABLE nuage_geom_omm (octas double precision,geometry geometry,octas_class integer,date text,time text);"
    dbExecute(DB_pol_lum, requete_nuage_omm)
    print(paste("Creation de la Base de donnee " ))
  }
  
  requete_sql <- paste("SELECT  count(distinct(ngo.time)) as nb_value FROM nuage_geom_omm as ngo WHERE ngo.date = \'",date,"\'", sep ="")
  Donnee_BDD <- dbGetQuery(DB_pol_lum, requete_sql) 
  if (Donnee_BDD < 24){
    
    for(heure in 0:23){ # seq(00, 23, by = 3)
      # heure <- 12
      # date <- date_debut
      requete_sql2 <- paste("SELECT  count(*) as nb_value FROM nuage_geom_omm as ngo
                            WHERE ngo.date = '",date,"'
                            AND ngo.time = '",sprintf("%02d", heure),":00:00'", sep ="")
      Donnee_BDD_2 <- dbGetQuery(DB_pol_lum, requete_sql2) 
      if(Donnee_BDD_2 == 0){
        date_fin <- paste(year(date),"-",sprintf("%02d", month(date)),"-",sprintf("%02d", day(date)),"T",sprintf("%02d", heure),":00:00Z", sep ="")
        # print(paste("REF :",date_ref, "_____________  ACTUAL :", date_fin))
        
        url <- paste("https://geoservices.meteofrance.fr/api/",token,
                     "/MF-NWP-GLOBAL-ARPEGE-01-EUROPE-WCS?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&format=image/tiff&coverageId=LOW_CLOUD_COVER__GROUND_OR_WATER_SURFACE___",
                     date_ref,"&subset=time(",date_fin,")&",
                     "subset=lat(44.717940438742204,46.700972665304704)&subset=long(3.115126052012159,7.081190505137159)", sep = "")
        
        # print(url)
        nom_fichier <- paste0("Geotiff_LOW_CLOUD_date_ref",year(date),"_",month(date),"_",day(date),"_H",sprintf("%02d", heure),".tiff")
        save_fichier <- paste0(dossier_enregistrement_geotiff,nom_fichier)
        
        setwd(file.path(dossier_enregistrement_geotiff))
        # getwd()
        
        download.file(url,destfile= nom_fichier, mode = 'wb')
        Sys.sleep(5)
        
        print(paste("GeoTiff enregistrer sous :", getwd(), " Au nom de : ",nom_fichier))
        
        polygone <- nom_fichier %>%
          raster() %>%
          rasterToPolygons() %>% #fasterVectorize( vectType='area', grassDir=grassDir) %>%
          st_as_sf() %>%
          rename_at(1,~"octas") %>%
          mutate(octas_class = round(octas, -1), # round -1 pour faire des classes tout les 10 octats
                 date = date,
                 time = paste(sprintf("%02d", heure),":00:00", sep ="") ) #%>%
        # group_by(octas, date, time) %>% # Si on fait des classe tout les 10 alors on peu grouper les geometries
        # summarise(nb_geom = n() ) %>%
        # filter(octas < 80)
        
        st_write(obj = polygone, dsn = DB_pol_lum, layer = "nuage_geom_omm", append = TRUE)
        file.remove(nom_fichier)
        print(paste(nrow(polygone), "Donnees enregistees dans la BDD"))
        Sys.sleep(60)
      }
    }
    
    }else {
      print(paste("Donnees dejà presente"))
    }
 
}


# . -------------------------------------------------------------------------- =============
# 5 - lancer le telechargement des donnees dans le dossier et sur BDD  ====
# . -------------------------------------------------------------------------- =============


date <- date_debut

while(date < date_fin){

  for(heure in c("00","03","06","09","12","15","18","21")){
    print(paste("Lancement telechargement donnees meteo france du : ",day(date),"/",month(date),"/",year(date)," à ",heure,"h" ))
    Requete_meteo_france_omm(date,heure)
    Sys.sleep(5)
  }
  date <- date %m+% days(1)
}


# . -------------------------------------------------------------------------- =============
# 6 - Ajout des stations de mesure ====
# . -------------------------------------------------------------------------- =============

if (!dbExistsTable(DB_pol_lum, "site_omm")){ #si BDD n'existe pas alors :
  
  # recherche du fichier sur le site internet 
  url <- "https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/postesSynop.csv"
  nom_fichier <- "C:/Users/fa101525/Desktop/Projet_Pollux/DATA_METEO/synop/postesSynop.csv"
  
  if (!file.exists(nom_fichier)){ #si le fichier n'est pas telecharge alors :
    download.file(url,destfile= nom_fichier, mode = 'wb')
    Sys.sleep(1)
  }

  # lecture du fichier en format geom
  site_mesure <- read.csv(nom_fichier, sep=";",na.strings="mq")%>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
      #mapview::mapview(site_mesure)
  
  # enregistrement du fichier sur la bdd 
  st_write(obj = site_mesure, dsn = DB_pol_lum, layer = "site_omm", append = TRUE)

} 

# . -------------------------------------------------------------------------- =============
# 7 - telechargement des Geotiff des nuages ====
# . -------------------------------------------------------------------------- =============

date <- date_debut

while(date < date_fin){
  Requete_meteo_france_nuage(date)
  date <- date %m+% days(1)
}
