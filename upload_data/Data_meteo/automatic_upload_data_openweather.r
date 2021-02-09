
# . -------------------------------------------------------------------------- =============
# 0 - Lecture des donnees openweathermap.org ====
# . -------------------------------------------------------------------------- =============

# Le script a pour objectif d'interroger la base de donnee openweather toutes les heures pendant 10h pour lire les donnees meteos. 
# une fois les donnees lu elle sont ensuite integre a la base de donnees 



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
lecture_openweather <- function(lat, long) {
donnee_openweather<- data.frame() # preparation d'un tableau 

requete <- paste("http://api.openweathermap.org/data/2.5/weather?lat=",lat,"&lon=",long,"&APPID=0a7ec99b7b8f5211e856953a0b07961f", sep = "")
result <- fromJSON(file = requete)

donnee_openweather <- as.data.frame(result)%>%
  mutate(date = Sys.time()) %>%
  st_as_sf(coords = c("coord.lon", "coord.lat"), crs = 4326)


print(paste("Heure de recuperation :",Sys.time()))

if (dbExistsTable(DB_pol_lum, "donnee_openweather")){ #si elle existe alors : 
  st_write(obj = donnee_openweather, dsn = DB_pol_lum, layer = "donnee_openweather", append = TRUE)
} else{ #si elle existe pas alors :
  st_write(obj = donnee_openweather, dsn = DB_pol_lum, layer = "donnee_openweather")
}

}




# . -------------------------------------------------------------------------- =============
# 4 - lancer l'acquisition des donnees  ====
# . -------------------------------------------------------------------------- =============

# Date de fin d'acquisition des donnees 
end_date <- "2020-11-30 15:45:20 CET"

# Frequence d'importation des donnees (en seconde, 1h = 3600, 30min = 1800, 15min = 900 )
freq <- 900
date <- Sys.time()
while(date < end_date){

  lecture_openweather(45.73,5.08)
  lecture_openweather(45.75,5.05)
  lecture_openweather(45.77,5.02)

  Sys.sleep(freq)
  date <- Sys.time()
}
