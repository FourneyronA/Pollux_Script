
# . -------------------------------------------------------------------------- =============
# 0 - Lecture des données openweathermap.org ====
# . -------------------------------------------------------------------------- =============

# Le script a pour objectif d'interroger la base de donnée openweather toutes les heures pendant 10h pour lire les données météos. 
# une fois les données lu elle sont ensuite intégré à la base de données 



# . -------------------------------------------------------------------------- =============
# 1 - Librairie ====
# . -------------------------------------------------------------------------- =============

library(RPostgreSQL) 
library(stringr)
library(lubridate)
library(tidyverse)
library(rjson)
library(rvest)

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
                        user = "postgres", password = "benjibenji",
                        options="-c search_path=meteo") # idem pour use

# . -------------------------------------------------------------------------- =============
# 3 - fonction lecture API/ecriture BDD ====
# . -------------------------------------------------------------------------- =============
lecture_openweather <- function(lat, long) {

donnee_openweather<- data.frame() # preparation d'un tableau 

requete <- paste("http://api.openweathermap.org/data/2.5/weather?lat=",lat,"&lon=",long,"&APPID=0a7ec99b7b8f5211e856953a0b07961f", sep = "")
result <- fromJSON(file = requete)

donnee_openweather <- as.data.frame(result)%>%
  mutate(date = Sys.time())
  

print(paste("Heure de récupération :",Sys.time()))

if (dbExistsTable(DB_pol_lum, "donnee_openweather")){ #si elle existe alors : 
  st_write(obj = donnee_openweather, dsn = DB_pol_lum, layer = "donnee_openweather", append = TRUE)
} else{ #si elle existe pas alors :
  st_write(obj = donnee_openweather, dsn = DB_pol_lum, layer = "donnee_openweather")
}

}




# . -------------------------------------------------------------------------- =============
# 4 - lancer l'acquisition des données  ====
# . -------------------------------------------------------------------------- =============

# Date de fin d'acquisition des données 
end_date <- "2020-11-24 11:27:20 CET"

# Fréquence d'importation des données (en seconde, 1h = 3600, 30min = 1800, 15min = 900 )
freq <- 
date <- Sys.time()
while(date < end_date){

  lecture_openweather(45.73,5.08)
  lecture_openweather(45.75,5.05)
  lecture_openweather(45.77,5.02)

  Sys.sleep(10)
  date <- Sys.time()
}
