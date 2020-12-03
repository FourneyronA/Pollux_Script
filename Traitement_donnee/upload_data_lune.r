
# . -------------------------------------------------------------------------- =============
# 0 - Lecture des donnees www.timeanddate.com ====
# . -------------------------------------------------------------------------- =============

# scrapping des donnees du site https://www.timeanddate.com, pour obtenir les differentes phases lunaires
# debut du levee de la lune, coucher de la lune, illumination, distances 
# par rapport a une ville entre deux dates

lieux = "saint-etienne"
date_debut = as.Date(x ="01/10/2020", format = c("%d/%m/%Y"))
date_fin = as.Date(x ="01/04/2021", format = c("%d/%m/%Y"))


# . -------------------------------------------------------------------------- =============
# 1 - Librairie ====
# . -------------------------------------------------------------------------- =============

library(sf)
library(RPostgreSQL) 
library(stringr)
library(lubridate)
library(tidyverse)
library(rjson)
library(rvest)

# . -------------------------------------------------------------------------- =============
# 2 - Connexion BDD postGIS ====
# . -------------------------------------------------------------------------- =============

## Supressions de toutes les connexions precedentes
lapply(dbListConnections(drv = dbDriver("PostgreSQL")),
       function(x) {dbDisconnect(conn = x)})

# type de connexion PostgreSQL
drv <- dbDriver("PostgreSQL")

# Creation de la connexion
source("C:/Users/fa101525/Desktop/GitHub/connect_bdd.R")


# . -------------------------------------------------------------------------- =============
# 3 - fonction lecture API/ecriture BDD ====
# . -------------------------------------------------------------------------- =============
Requete_donnee_lune <- function(date, localisation) {
  # date <- date_debut
  # localisation = "saint-etienne"
  mois <- month(date) #recuperation du mois 
  annee <- year(date) #recuperation de l'annee 
  
  
  # Adresse web avec interrogation sur les donnees du lieux et date
  lien_site <-paste("https://www.timeanddate.com/moon/france/",localisation,"?month=",mois,"&year=",annee, sep ="")
  
  # print(paste("mois : ",mois))
  # print(paste("annee : ",annee))
  # print(paste("localisation : ",localisation))
  # print(paste("lien site : ",lien_site))
  
  # Extraction de la page html 
  premier_page <- html_session(lien_site) %>%
    read_html()
  
  # Extraction de toute les balises <section>
  part_section <- premier_page %>%
    html_nodes("body") %>%
    html_nodes("main") %>%
    html_nodes("article") %>%
    html_nodes("section") 
  
  # Extraction de toute les balises <div>
  part_div <- part_section[2] %>% # choix de la 2eme section
    html_nodes("div") 
  
  # Extraction et mise en forme du tableau 
  df_part_tab <- part_div[4] %>% # choix de la 4eme div
    html_nodes("table") %>% # Extraction du tableau
    html_table() %>% # lecture des donnees tableau
    data.frame # mise en forme de dataframe
  
  # mise en forme des donnees 

  donnee_lune <- df_part_tab
  names(donnee_lune) <- c("jours","Moonrise_heure","Moonrise_alti",
                                  "Moonset_heure","Moonset_alti",
                                  "Moonrise_heure2","Moonrise_alti2",
                                  "time_heure","time_alti",
                                  "distance","illumination")
  donnee_lune <- donnee_lune %>%
    mutate(jours = as.numeric(jours) ) %>% ## recupere le numero du jours et met NA pour les lignes qui contiennent des notes
    drop_na() %>%# supressions des notes 
    mutate(date = as.Date(x =paste(jours,mois,annee, sep = "/"), format = c("%d/%m/%Y") ),
           moonrise = ifelse(Moonrise_heure != "-", Moonrise_heure, ifelse(Moonrise_heure2 != "-", Moonrise_heure2, "00 h 00" )),
           moonset = ifelse(Moonset_heure != "-", Moonset_heure, "00 h 00" ),
           distance =  as.numeric(str_replace(distance, " ", "")),
           illumination = as.double(str_replace(str_replace(illumination, "%", ""), ",", ".")),
           ville = localisation)
  
  donnee_lune <- donnee_lune[,c(12,13,14,15,10,11)]
  
### moyenne des distance et luminausitee pour valeur manquante 
  for(i in 1:nrow(donnee_lune)){
   
    if(is.na(donnee_lune[i,6])){
      donnee_lune[i,6] <- round(((donnee_lune[(i-1),6] + donnee_lune[(i+1),6]) / 2),1)
      donnee_lune[i,5] <- round(((donnee_lune[(i-1),5] + donnee_lune[(i+1),5]) / 2),0)
      }
    
  }
  
  
  print(paste("Implementation des donnees lunaire du :",mois,"/",annee, sep = "" ))
  
  if (dbExistsTable(DB_pol_lum, "donnee_lune")){ #si elle existe alors :
    st_write(obj = donnee_lune, dsn = DB_pol_lum, layer = "donnee_lune", append = TRUE)
  } else{ #si elle existe pas alors :
    st_write(obj = donnee_lune, dsn = DB_pol_lum, layer = "donnee_lune")
  }

}


# . -------------------------------------------------------------------------- =============
# 4 - lancer l'acquisition des donnees  ====
# . -------------------------------------------------------------------------- =============
date <- date_debut

while(date < date_fin){
  Requete_donnee_lune(date,lieux)
  date <- date %m+% months(1)
}

