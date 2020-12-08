
# . -------------------------------------------------------------------------- =============
# 1 - Lecture des donnees Meteo France ====
# . -------------------------------------------------------------------------- =============

# Le script a pour objectif de convertir les donnees meteo france disponible en CSV dans une BDD spatiale. 

# Nous avons deux type de donnees : 
# - Les stations de mesures georeferencer : https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/postesSynop.csv
# - Les mesures des stations : https://donneespubliques.meteofrance.fr/efond=produit&id_produit=90&id_rubrique=32

# Prochain objectif : telechargement automatiser des geotiff dans la BDD

# url <- "https://geoservices.meteofrance.fr/api/__P9a9AG1DlYVB78ayodw5qqTgnb_1ty9WRy3oeZjBNSg__/MF-NWP-GLOBAL-ARPEGE-01-EUROPE-WCS?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&format=image/tiff&coverageId=LOW_CLOUD_COVER__GROUND_OR_WATER_SURFACE___2020-12-03T00:00:00Z&subset=time(2020-12-03T09:00:00Z)&subset=lat(45.085090577293315,46.076606690574565)&subset=long(4.002749385542207,5.985781612104707)"

dossier_enregistrement_csv <- "C:/Users/fa101525/Desktop/Projet_Pollux/DATA_METEO/synop/test_automatik_save"
#dossier_enregistrement_tiff <- "C:/Users/fa101525/Desktop/Projet_Pollux/DATA_METEO/synop/test_automatik_save"

### / ! \ VARIABLE A PARAMETRER / ! \ 

# dossier ou serons stocker les fichiers CSV
dossier_enregistrement_csv <- "C:/Users/fa101525/Desktop/Projet_Pollux/DATA_METEO/synop/test_automatik_save"

# Date de debut et fin a telecharger (de base la semaine précédante)
date_fin = Sys.Date()%m-% days(7) #
date_debut = date_fin %m-% days(7)

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


# . -------------------------------------------------------------------------- =============
# 2 - Connexion BDD postGIS ====
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
# 3 - fonction lecture API/ecriture BDD ====
# . -------------------------------------------------------------------------- =============

# fonction qui permet d'interroger le site meteo france 
# conversion du format des donnees 
# enregistrement sur la base de donnees
Requete_meteo_france_omm <- function(date, horaire) {
  
  
  ligne_link <- paste0("https://donneespubliques.meteofrance.fr/?fond=donnee_libre&prefixe=Txt%2FSynop%2Fsynop&extension=csv&date=",date,"&reseau=",horaire)%>%
    getURL() %>%
    textConnection() %>%
    read.csv() %>%
    filter(str_detect(.[,1], "cliquez sur le lien suivant"))
  
  debut_add <- str_locate(ligne_link, "donnees_libres")[1]
  fin_add <- str_locate(ligne_link, ".csv")[2]
  
  url <- str_sub(ligne_link, start = debut_add , end = fin_add)
  url <- paste0("https://donneespubliques.meteofrance.fr/",url)
  
  print(url)
  
  nom_fichier <- paste0("date_",date,"heure_",horaire,".csv")
  download.file(url,destfile= nom_fichier, mode = 'wb')
  Sys.sleep(1)
  fichier_2 <- read.csv(nom_fichier, sep=";",na.strings="mq")
  
  if (ncol(fichier_2) < 2) {
    print(paste("DONNEES NON DISPONIBLE AU : ",day(date),"/",month(date),"/",year(date)," à ",horaire,"h" ))
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
             date_ok = paste( str_sub(date, start = 1, end = 4),"-",   ## extraction annee
                              str_sub(date, start = 5, end = 6),"-",   ## extraction mois
                              str_sub(date, start = 7, end = 8)," ",   ## extraction jour
                              str_sub(date, start = 9, end = 10),":",  ## extraction heure
                              str_sub(date, start = 11, end = 12),":", ## extraction minutes 
                              str_sub(date, start = 13, end = 14)," UTC",  sep="")) ## extraction seconde
    
    donnee_omm <- donnee_omm[ ,c("numer_sta","date","date_ok","pmer","tend","cod_tend","dd","ff",
                                 "t","td","u","vv","ww","w1","w2","n","nbas","hbas","cl","cm",
                                 "ch","pres","niv_bar","geop","tend24","tminsol","raf10","etat_sol",
                                 "ht_neige","ssfrai","perssfrai","nnuage1","ctype1","hnuage1",
                                 "nnuage2","ctype2","hnuage2",
                                 "nnuage3","ctype3","hnuage3",
                                 "nnuage4","ctype4","hnuage4")]
    
    if (dbExistsTable(DB_pol_lum, "donnee_omm")){ #si BDD existe alors :
      
      requete_sql <- paste("SELECT count(*) FROM donnee_omm WHERE date =",paste0(date,horaire,"0000"))
      Donnee_BDD <- dbExecute(DB_pol_lum, requete_sql) 
      if (Donnee_BDD == 0){ # verification si la donnee existe deja dans la BDD
        st_write(obj = donnee_omm, dsn = DB_pol_lum, layer = "donnee_omm", append = TRUE)
        print(paste("Donnees enregistrer dans la BDD" ))
      } else{
        print(paste("Donnees déjà présente dans la BDD" ))
      }
    } else{ #si BDD existe pas alors :
      requete_fichier_donnee_omm ="CREATE TABLE donnee_omm (
                        numer_sta integer,
                        date double precision,
                        date_ok text,
                        pmer integer,
                        tend integer,
                        cod_tend integer,
                        dd integer,
                        ff double precision,
                        t double precision,
                        td double precision,
                        u integer,
                        vv integer,
                        ww integer,
                        w1 integer,
                        w2 integer,
                        n integer,
                        nbas integer,
                        hbas integer,
                        cl integer,
                        cm integer,
                        ch integer,
                        pres integer,
                        niv_bar integer,
                        geop integer,
                        tend24 integer,
                        tminsol double precision,
                        raf10 double precision,
                        etat_sol double precision,
                        ht_neige double precision,
                        ssfrai double precision,
                        perssfrai integer,
                        nnuage1 integer,
                        ctype1 integer,
                        hnuage1 integer,
                        nnuage2 integer,
                        ctype2 integer,
                        hnuage2 integer,
                        nnuage3 integer,
                        ctype3 integer,
                        hnuage3 integer,
                        nnuage4 integer,
                        ctype4 integer,
                        hnuage4 integer);"
      
      dbExecute(DB_pol_lum, requete_fichier_donnee_omm)
      print(paste("Creation de la Base de donnée " ))
      st_write(obj = donnee_omm, dsn = DB_pol_lum, layer = "donnee_omm", append = TRUE)
      print(paste("Donnees enregistrer dans la BDD" ))
    }
  }
  
}

# . -------------------------------------------------------------------------- =============
# 4 - lancer le telechargement des donnees dans le dossier et sur BDD  ====
# . -------------------------------------------------------------------------- =============

setwd(file.path(dossier_enregistrement_csv))
date <- date_debut

while(date < date_fin){
  date_csv <- paste0(year(date),month(date),day(date))
  
  for(heure in c("00","03","06","09","12","15","18","21")){
    print(paste("Lancement telechargement donnees meteo france du : ",day(date),"/",month(date),"/",year(date)," à ",heure,"h" ))
    Requete_meteo_france_omm(date_csv,heure)
  }
  
  date <- date %m+% days(1)
}


# . -------------------------------------------------------------------------- =============
# 5 - Ajout des stations de mesure ====
# . -------------------------------------------------------------------------- =============

if (!dbExistsTable(DB_pol_lum, "site_omm")){ #si BDD n'existe pas alors :
  
  # recherche du fichier sur le site internet 
  url <- "https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/postesSynop.csv"
  nom_fichier <- "C:/Users/fa101525/Desktop/Projet_Pollux/DATA_METEO/synop/postesSynop.csv"
  
  if (!file.exists(nom_fichier)){ #si le fichier n'est pas téléchargé alors :
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
