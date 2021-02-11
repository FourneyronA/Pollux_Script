
# . -------------------------------------------------------------------------- =============
# 0 - Lecture des donnees Meteo France ====
# . -------------------------------------------------------------------------- =============

# Le script a pour objectif de convertir les donnees meteo france dans une BDD spatiale (les données peuvent être des fichier local, ou des requête sur le web). 
# ATTENTION il est imperatif d'adapter les parties suivantes : 
# 2 - Variable et parametre : (emplacement des fichiers)
# 3 - Connexion BDD postGIS : (connexion a votre base de donnee)

# Nous avons deux type de donnees : 
# - Les stations de mesures georeferencer : https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/postesSynop.csv
# - Les mesures des stations : https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=90&id_rubrique=32
# - Les deplacements des nuages gaoreferencer : https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=130&id_rubrique=51

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

dossier_enregistrement_csv <- "C:/Users/fa101525/Desktop/Projet_Pollux/DATA_METEO/synop/test_automatik_save"

# enregistrement du TOKEN pour requeter les donnees de meteo france 
source("C:/Users/fa101525/Desktop/GitHub/token_meteofrance.R")

# Date de debut et fin a telecharger (en fonction de la date du jour) 
# Les donnes ne sont diponibles que pendant un certain moment, il faut donc relancer le script tout les 4 jours maximum
# Le script peut etre lancer tout les jours, il verifie si les donnees sont presente ou non avant de lancer le telechargement
# Si vos donnees sont enregistrer dans un dossier en local, n'hesitez pas a indiquer les dates que vous souhaitez.

date_debut = Sys.Date() %m-% days(4) # Debut 4 jours avant aujourd'hui
date_fin = date_debut %m+% days(3)   # Fin 1 jours avant aujourd'hui

# . -------------------------------------------------------------------------- =============
# 3 - / ! \ Connexion BDD postGIS / ! \  ====
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
  
  # Preparation des variables sous la bonne formes 
  annee <-  as.numeric(year(date)) # YYYY
  mois <- sprintf("%02d", as.numeric(month(date))) # MM
  jours <- sprintf("%02d", as.numeric(day(date))) # JJ
  # Preparation emplacement
  setwd(file.path(dossier_enregistrement_csv))
  
  # On verifie que la table existe dans la BDD Si elle n'existe pas alors on la créer 
  if (!dbExistsTable(DB_pol_lum, "donnee_omm")){
    # Preparation de la requete SQL
    requete_fichier_donnee_omm ="CREATE TABLE donnee_omm (numer_sta integer,date double precision,date_ok text,time_ok text,pmer integer,tend integer,cod_tend integer,dd integer,ff double precision,
                                                            t double precision,td double precision,u integer,vv integer,ww integer,w1 integer,w2 integer,n integer,nbas integer,hbas integer,
                                                            cl integer,cm integer,ch integer,pres integer,niv_bar integer,geop integer,tend24 integer,tminsol double precision,raf10 double precision,
                                                            etat_sol double precision,ht_neige double precision,ssfrai double precision,perssfrai integer,nnuage1 integer,ctype1 integer,hnuage1 integer,
                                                            nnuage2 integer,ctype2 integer,hnuage2 integer,nnuage3 integer,ctype3 integer,hnuage3 integer,nnuage4 integer,ctype4 integer,hnuage4 integer);"
    # Execution de la requete
    dbExecute(DB_pol_lum, requete_fichier_donnee_omm)
    # Message d'avertissement 
    print(paste("Creation de la Base de donnee " ))
  }
  
  # Requête pour savoir combien de données sont présente a la date et l'heure du script
  requete_sql <- paste("SELECT count(*) FROM donnee_omm 
                        WHERE date_ok = '",date,"'
                        AND time_ok = '",horaire,":00:00'",sep ="")
  Donnee_BDD <- dbGetQuery(DB_pol_lum, requete_sql) 
  if (Donnee_BDD == 0){ # Si aucune données est présente alors on lance le téléchargement 
    
    # Preparation du nom du fichier 
    file_name <- paste(annee,mois,jours, sep = "_")
    file_omm <- paste0(file_name, "/donnee_omm",  sep = "")
    
    nom_fichier_csv <- paste0(file_omm,"/","date_",date,"heure_",horaire,".csv")
    
    # On verifier que le fichier n'as pas été téléchargé en local 
    if(file.exists(nom_fichier_csv)){ 
      # Si oui on viens lire le fichier 
      print(paste("Fichier disponible dans le repertoire : ",nom_fichier_csv))
      fichier_2 <- read.csv(nom_fichier_csv, sep=";",na.strings="mq")
      
    } else{ # Sinon on viens téléchargé via l'adresse URL
      # On viens interroger le site web pour avoir le lien de telechargement 
      # ligne_link <- paste0("https://donneespubliques.meteofrance.fr/?fond=donnee_libre&prefixe=Txt%2FSynop%2Fsynop&extension=csv&date=",date,"&reseau=",horaire)%>%
      #   getURL() %>% # interrogation de URL a la date et heure parametrer 
      #   textConnection() %>% # verification que URL est OK
      #   read.csv() %>% # Lecture du resultats
      #   filter(str_detect(.[,1], "cliquez sur le lien suivant")) # Detection du lien 
      # 
      # 
      # debut_add <- str_locate(ligne_link, "donnees_libres")[1] # position dans la chaine de caractere du mot donnees libres
      # fin_add <- str_locate(ligne_link, ".csv")[2]            # position dans la chaine de caractere de l'extention du fichier 
      # link <- str_sub(ligne_link, start = debut_add , end = fin_add) # extraction de l'URL
      # link2 <- str_remove_all(link, "-") #supression des caactere 
      
      #Creation de l'URL qui adopte au finale toujours la même forme generique donc pas besoins de passer par l'interrogation du site web 
      url <- paste("https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/synop.",annee,mois,jours,horaire,".csv",sep ="")
      #print(url)
      
      # Nom du fichier 
      nom_fichier <- paste0("date_",date,"heure_",horaire,".csv")
      # Lancement du téléchargement 
      download.file(url,destfile= nom_fichier, mode = 'wb',quiet = TRUE )
      # Temps de pose 
      Sys.sleep(1)
      # Lecture du fichier 
      fichier_2 <- read.csv(nom_fichier, sep=";",na.strings="mq")
      # Supression du fichier
      file.remove(nom_fichier)
    }

    if (ncol(fichier_2) < 2) {
      # si le nombre de champs est inferrieur a 2 alors les données lu sont inccorect 
      print(paste("DONNEES NON DISPONIBLE AU : ",date," a ",horaire,"h" ))
 
    } else { # On estime que le fichier est correct on viens alors le remettre en forme 
      list_colum <- colnames(fichier_2) %>%
        unlist() %>%
        paste( collapse = ' ')
      # on force la création de colonne qui n'existe pas forcement, mais si elle n'existe pas on leurs attribue la valeur NA 
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
      #On selection uniquement une partie des colonnes qui nous interesse 
      donnee_omm <- donnee_omm[ ,c("numer_sta","date","date_ok","time_ok","pmer","tend","cod_tend","dd","ff","t","td","u","vv","ww","w1","w2","n","nbas","hbas","cl","cm","ch","pres","niv_bar","geop","tend24","tminsol","raf10","etat_sol","ht_neige","ssfrai","perssfrai","nnuage1","ctype1","hnuage1","nnuage2","ctype2","hnuage2","nnuage3","ctype3","hnuage3","nnuage4","ctype4","hnuage4")]
      # On ecrit le resultat de ses données dans la BDD 
      st_write(obj = donnee_omm, dsn = DB_pol_lum, layer = "donnee_omm", append = TRUE)
      # Message d'avertissement pour signaler que l'operation c'est bien derouler
      print(paste( nrow(donnee_omm)," Donnees enregistrer dans la BDD" ))
    }
    
  
  } else{
    # Message d'avertissement pour signaler que les données sont deja dans la BDD 
    print(paste("Donnees deja presente dans la BDD" ))
  }
}
# recuperation des geotiff disponible (TOKEN necessaire)
Requete_meteo_france_nuage <- function(date, emprise, type_donnee) { 
  # Variable test 
  # date <-  Sys.Date()%m-% days(4)
  # heure <- 11
  # emprise <- c("44.449999724439457","45.950000000000003","3.649999999999999","6.650000794500522")
  # type_donnee <- "LOW_CLOUD_COVER__GROUND_OR_WATER_SURFACE___"
  
  # Creation des variable a partie des variable d'entrée au bon format 
  annee <-  as.numeric(year(date))
  mois <- sprintf("%02d", as.numeric(month(date)))
  jours <- sprintf("%02d", as.numeric(day(date)))
  # Permet d'avoir un diminutif du type de donnée sur les info de type  "LOW_CLOUD_COVER__GROUND_OR_WATER_SURFACE___"
  pos_stop <- str_locate_all(type_donnee, "_")[[1]][2] # detection la position du deuxième "_" 
  type_name <- str_sub(type_donnee, 0, pos_stop)  # extraction de "LOW_CLOUD"
  # preparation de l'emplacement du fichier et du chemin de sauvegarde
  setwd(file.path(dossier_enregistrement_csv))
  file_name <- paste(annee,mois,jours, sep = "_")
  file_geotiff <- paste0(file_name, "/",type_name,"donnee_geotiff_", sep = "")
  
  date_ref <- paste(annee,"-",mois,"-",jours,"T00:00:00Z", sep ="")
  
  # test si dans la BDD la table geom_omm_nuage existe 
  if (!dbExistsTable(DB_pol_lum, "geom_omm_nuage")){
    # si elle n'existe pas alors on la créer 
    requete_nuage_omm ="CREATE TABLE geom_omm_nuage (geometry geometry,id_nuage integer);"
    dbExecute(DB_pol_lum, requete_nuage_omm)
    print(paste("Creation de la table : geom_omm_nuage " ))
  }
  # test si dans la BDD la table donnee_omm_nuage existe 
  if (!dbExistsTable(DB_pol_lum, "donnee_omm_nuage")){
    # si elle n'existe pas alors on la créer 
    requete_nuage_omm ="CREATE TABLE donnee_omm_nuage (octas double precision,id_nuage integer,type_nuage text,octas_class integer,date text,time text);"
    dbExecute(DB_pol_lum, requete_nuage_omm)
    print(paste("Creation de la table : donnee_omm_nuage " ))
  }
  
  # On viens verifier si il y a des données enregistrer a la date indiquer
  # On veux avoir toute les données pour chaque heure de la journée, il faut donc 24 heure différentes 
  requete_sql <- paste("SELECT  count(distinct(ngo.time)) as nb_value FROM donnee_omm_nuage as ngo WHERE ngo.date = \'",date,"\' AND type_nuage = \'",type_name,"\'", sep ="")
  Donnee_BDD <- dbGetQuery(DB_pol_lum, requete_sql) 
  if (Donnee_BDD < 24){ # Si il n'y a pas 24 heure différentes alors on chercher celle qui nous manque une par une 
    
    for(heure in 0:23){ # on par de 00h jusqu'à 23h
      # on viens requeter notre BDD pour : Verifier si on a les données de l'heure sur l'emprise indiquer
      # Pour cela il faut s'assurer de savoir combien de geometry est présente dans l'emprise et si via la jointure on a autant de donnée que de geometry 
      requete_nb_geom <- paste("SELECT count(*) as nb_value FROM geom_omm_nuage WHERE ST_Intersects(ST_Transform(geometry,4326), ST_GeomFromText(\'POLYGON((",
                           emprise[3]," ",emprise[1],",", emprise[3]," ",emprise[2],",", emprise[4]," ",emprise[2],",", emprise[4]," ",emprise[1],",", emprise[3]," ",emprise[1],"))\',4326) )",sep ="")
																		   
      nb_geom <- st_read(DB_pol_lum,query = requete_nb_geom) 
      nb_geom <- as.numeric(nb_geom$nb_value)

      requete_sql <- paste("SELECT count(*) as nb_value FROM donnee_omm_nuage as ngo WHERE ngo.date = \'",date, "\' AND type_nuage = \'",type_name,"\' AND time = \'",sprintf("%02d", heure),":00:00","\'", sep ="")
      nb_donnee_BDD <- dbGetQuery(DB_pol_lum, requete_sql) 
      nb_donnee_BDD <- as.numeric(nb_donnee_BDD$nb_value)
      
      
      if (nb_donnee_BDD == nb_geom){ # si il y a autant de geometry qui on des données a la date et l'heure dans la BDD c'est OK
        print(paste("Donnees deja presente"))
      }
      
      if (nb_donnee_BDD > 0 & nb_geom < nb_donnee_BDD){ # si il y a des données a la date et l'heure mais pas forcement pour l'ensemble des geometry a voir 
        print(paste("DONNEE SEMI PRESENTE CAS SPEFICIQUE A REFAIRE"))
        # le cas peut ce présenter si dans le même jours, il y a un changement d'emprise qui pourrais ce supperposer
        # le traitement deviens un peu lourd pour un cas très occasionnel pour nous le mieux est de télécharger ses données le lendemain avant les données habituelles/
      }
        
      if (nb_donnee_BDD == 0){ # si il n'y as aucune donnée alors on viens lancer la lecture des données 
      # Création des variable avec les nom spéficique referent 
      date_fin <- paste(annee,"-",mois,"-",jours,"T",sprintf("%02d", heure),":00:00Z", sep ="")
      nom_fichier_dl <- paste0(file_geotiff,"/Geotiff_",type_name,"date_ref",year(date),"_",month(date),"_",day(date),"_H",sprintf("%02d", heure),".tiff")
      
      if(file.exists(nom_fichier_dl)){ #Si le fichier existe en local on indique sont emplacement 
        nom_fichier <- nom_fichier_dl # emplacement de reference
        print(paste("GeoTiff disponible dans le repertoire :", nom_fichier))
        var_dl = 0 # on indique qu'il n'y a pas eu de telechargerpent via API
      }else { # Si le fichie est inexistant on lance la requête via API 
        # nom du fichier generique 
        nom_fichier <- paste0(dossier_enregistrement_csv,"/Geotiff_",type_name,"date_ref",year(date),"_",month(date),"_",day(date),"_H",sprintf("%02d", heure),".tiff")
        # preparation de l'URL
        url <- paste("https://geoservices.meteofrance.fr/api/",token,
                     "/MF-NWP-GLOBAL-ARPEGE-01-EUROPE-WCS?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&format=image/tiff&coverageId=",
                     type_donnee, date_ref,"&subset=time(",date_fin,")&",
                     "subset=lat(",emprise[1],",",emprise[2],")&subset=long(",emprise[3],",",emprise[4],")", sep = "")
        # Lancement du telechargement 
        download.file(url,destfile= nom_fichier,method = "wininet", mode = 'wb',quiet = TRUE ) 
        # Message d'avertissement 
        print(paste("GeoTiff enregistrer sous :", nom_fichier))
        # temp de repos 
        Sys.sleep(2)
        # on indique qu'on a du proceder a un telechargement 
        var_dl = 1
      }

      # une fois le fichier Geotiff téléchargé OU présent en locale on viens lire l'image pour :
      # mais il faut qu'on sache si les geomettry existe dejà pour cette image 
      # pour cela on viens lire les les geometry dans notre base de données 
      geom_omm_nuage <- st_read(DB_pol_lum,query = "SELECT * FROM geom_omm_nuage") 
      
      if(nrow(geom_omm_nuage) < 1){ # Si il n'y a acune geometry présente dans notre base :
        # Lecture du fichier tiff 
        full_data <- nom_fichier %>%
          raster() %>%
          rasterToPolygons() %>% #fasterVectorize( vectType='area', grassDir=grassDir) %>%
          st_as_sf() %>%
          rename_at(1,~"octas") %>%
          mutate(octas_class = round(octas, -1), # round -1 pour faire des classes tout les 10 octats
                 type_nuage = type_name,
                 date = date,
                 time = paste(sprintf("%02d", heure),":00:00", sep =""), 
                 id_nuage = row_number()) #Attribution d'un ID en fonction du nombre de ligne 
        
        # extraction d'une table contenant ID et geom
        geom_missing <- full_data[c(2,7)] 
        # enregistrement de cette table dans la BDD 
        st_write(obj = geom_missing, dsn = DB_pol_lum, layer = "geom_omm_nuage", append = TRUE)
        
      } else{ # Si il existe DEJA des geometry 
        # Lecture du fichier tiff 
        polygone <- nom_fichier %>%
          raster() %>% #lecture fichier raster 
          rasterToPolygons() %>% #Vectorisation du fichier 
          st_as_sf() %>% #mise au format SF 
          rename_at(1,~"octas") %>% #renomer la col 1 (information du raster)
          mutate(octas_class = round(octas, -1), # round -1 pour faire des classes tout les 10 octats
                 type_nuage = type_name, 
                 date = date,
                 time = paste(sprintf("%02d", heure),":00:00", sep =""))
        
        # jointure spatiale entre le tiff et le geometry enregistrer dans la BDD
        full_data <- polygone %>%
          st_centroid() %>% #passage des polygone du tiff en points
          st_join(geom_omm_nuage, by ="geometry") # joiture spatiale (points dans un polygone)
        # la jointure permet d'avoir l'ID des chaque geometry 
        
        # On viens extraire les données qui n'ont pas d'ID 
        data_na <- full_data %>%
          filter(is.na(id_nuage))
        
        if(nrow(data_na) > 0 ){  #Si il y a des geometry qui n'ont pas d'ID cela veut dire qu'elle ne sont pas dans la BDD et qu'il faut les ajouter 
          
          max <- max(full_data$id_nuage, na.rm=TRUE) # On recupere alors l'ID MAX de notre table 
          
          full_data <- polygone %>%
            st_centroid() %>% 
            st_join(geom_omm_nuage, by ="geometry") %>%
            mutate(id_nuage = ifelse(is.na(id_nuage),(row_number()+max),id_nuage) ) # Puis on viens créer un nouvelle ID pour les geometry qui n'en on pas sinon on garde l'ancien 
          
          # extraction d'une table contenant ID et geom
          geom_missing <- full_data[c(2,7)]  
          # enregistrement de cette table dans la BDD ( ajoute seulement les geometry/ID inexistant)
          st_write(obj = geom_missing, dsn = DB_pol_lum, layer = "geom_omm_nuage", append = TRUE)
          
        }
        
      }
      
      
      # On supprime les geometry de la table pour garder seulement les donnée et l'ID qui permet de faire la jointure entre les deux 
      data_polygone <- st_drop_geometry(full_data)
      # Puis on enregistre les données dans la BDD 
      st_write(obj = data_polygone, dsn = DB_pol_lum, layer = "donnee_omm_nuage", append = TRUE)
      # On choisie un chiffre aléatoire entre 5 et 15
      nb <- sample(c(5:15), 1) 
      # Message d'avertissement du bon deroulement et du chiffre aleatoire choisie 
      print(paste(nrow(full_data), "Donnees enregistees dans la BDD _______________________ alea", nb))
      # Si on est passé par le telechargement des données via l'API
      if(var_dl == 1){
        # On suprime le fichier que l'on a telechagé 
        file.remove(nom_fichier)
        # On temporise de manière aléatoire le prochaine telechargement 
        Sys.sleep(nb)
        # Parfois le site peu nous bloquer l'accès de manière temporaire en cas de surmenage de l'API
        # la temporisation aléatoire permet d'éviter ça, cependant il arrive quand même que la situation arrive 
        # Si le cas est present un message d'erreur va apparaitre '403 ...' il faut alors relancer le script (parfois il faut le relance 2 ou 3 fois )
      }
     
      }
    }
    
  }else {
    # Message d'avertissement 
    print(paste("Donnees deja presente"))
  }
  
}

# . -------------------------------------------------------------------------- =============
# 5 - lancer le telechargement des donnees des CAPTEUR OMM sur BDD  ====
# . -------------------------------------------------------------------------- =============

# on initialise la variable date au debut de notre période de telechargement 
date <- date_debut

while(date < date_fin){ # Puis pour chaque jours entre date début/datefin 
  # On boucle sur les heures de la journée (toute les 03 heures, c'est la fréquence d'aquisition des données des capteurs )
  for(heure in c("00","03","06","09","12","15","18","21")){ 
    # Message d'avertissement pour savoir on l'on en ai 
    print(paste("Lancement telechargement donnees meteo france du : ",day(date),"/",month(date),"/",year(date)," a ",heure,"h" ))
    # Lancement de la requête de collect ou lecture sur fichier locale des données 
    Requete_meteo_france_omm(date,heure)
  }
  date <- date %m+% days(1)
}

# Les capteurs OMM sont localiser a des endroit précis dans toute la france et en outre mer
# On viens ici verifier que nous avons bien la localisation dans notre base de données 

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

  # enregistrement du fichier sur la bdd 
  st_write(obj = site_mesure, dsn = DB_pol_lum, layer = "site_omm")
  
} 

# . -------------------------------------------------------------------------- =============
# 6 - telechargement des Geotiff des nuages ====
# . -------------------------------------------------------------------------- =============

# Même méthode mais cette fois avec les données geotiff 
# Ici on lance de telechargement sur une emprise centrer sur Grenoble
# sur les données LOW_CLOUD (octas en % des nuage bas < 2500m ) et TOTAL_CLOUD (Octas en % de l'ensemble des nuages)

# D'autre données sont dispo 
# type_donnee <- "LOW_CLOUD_COVER__GROUND_OR_WATER_SURFACE___" (Octas en % des nuage < 2500m)
# type_donnee <- "MEDIUM_CLOUD_COVER__GROUND_OR_WATER_SURFACE___" (Octas en % des nuage [2500 et 5000m])
# type_donnee <- "TOTAL_CLOUD_COVER__GROUND_OR_WATER_SURFACE___" (Octas en %  de l'ensemble des nuages)
# type_donnee <- "HIGH_CLOUD_COVER__GROUND_OR_WATER_SURFACE___" (Octas en % des nuage > 5000m)
# type_donnee <- "TOTAL_SNOW_PRECIPITATION__GROUND_OR_WATER_SURFACE___" (chute des neiges )

date <- date_debut

while(date < date_fin){
  # Zone d'étude 
  emprise <- c("44.449999724439457","45.950000000000003","3.649999999999999","6.650000794500522")
  # Telechargement des nuages bas (< 2500m altitude)
  Requete_meteo_france_nuage(date,emprise, "LOW_CLOUD_COVER__GROUND_OR_WATER_SURFACE___")
  # Telechargement des octas en générale (tout nuage confondue)
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
