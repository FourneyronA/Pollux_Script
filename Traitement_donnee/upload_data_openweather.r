
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
# 3 - Traitements des fichiers ====
# . -------------------------------------------------------------------------- =============


donnee_openweather<- data.frame() # preparation d'un tableau 

for( i in 1:10){ 
  result <- fromJSON(file ="http://api.openweathermap.org/data/2.5/weather?lat=45.7265&lon=5.077833&APPID=0a7ec99b7b8f5211e856953a0b07961f")
  json_data_frame <- as.data.frame(result)
  
  json_data_frame <- json_data_frame %>%
    mutate(date = Sys.time(),
           date_2 = lubridate::hms(paste(hour(date),minute(date),second(date),sep = ":" )))

    donnee_openweather <- donnee_openweather %>%
    rbind(json_data_frame)
  
  print(paste("Heure de récupération :",Sys.time()))
  print(paste("Nombre de donnée :",nrow(donnee_openweather)))
  
  ggplot(donnee_openweather)+
    geom_line(aes(y= round(main.temp-273.15,2) , x = date_2 ), color = "red", lwd = 2, alpha = 0.8)+
    geom_point(aes(y= round(main.temp-273.15,2) , x = date_2 ), color = "black", size = 4)+
    labs(title = "Temperature à Saugnieu le 19/11/2020", x = "Heure de la journée", y = "Degré Celsius" )+
    scale_x_time()+
    theme_minimal()+
    theme(axis.text.x = element_text(angle=45, hjust = 1))
  
  ggsave(file = paste("C:/Users/fa101525/Desktop/Projet_Pollux/DATA_METEO/JSON_openweather/",nrow(donnee_openweather),".png", sep =""))
  
  Sys.sleep(3600)
}



#enregistrement dans la BDD
st_write(obj =donnee_omm, dsn = DB_pol_lum, layer = "donnee_omm")

