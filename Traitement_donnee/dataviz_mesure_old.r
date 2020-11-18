# . -------------------------------------------------------------------------- =============
# 0 - DataVisualisation des données issues des capteurs de mesures ====
# . -------------------------------------------------------------------------- =============

# Le script a pour objectif de lire les données fournirs par les capteurs de mesures et de les enregistrer dans la BDD


# . -------------------------------------------------------------------------- =============
# 1 - Librairie ====
# . -------------------------------------------------------------------------- =============

library(sf)
library(RPostgreSQL) 
library(readr)
library(stringr)
library(lubridate)
library(tidyverse)
library(viridis)


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
# 3 - Lecture des données  ====
# . -------------------------------------------------------------------------- =============

Donnee_NSB <- dbGetQuery(DB_pol_lum, "SELECT date, time, nsb
                                      FROM donnee_capteur") 

# . -------------------------------------------------------------------------- =============
# 4 - Visualisation des données  ====
# . -------------------------------------------------------------------------- =============

# Visualisation sur la journée du 03/27

DATA_03_27 <- Donnee_NSB %>%
  mutate(date_ok = as_date(paste("2020/",str_sub(date, start = 6, end = 10),sep = "")),
         time_ok = hms::as_hms(time)) %>%
  filter(date_ok >= as.Date("2020-04-01") & date_ok <= as.Date("2020-04-30"))%>%
  filter(time_ok <= hms::as_hms("06:00:00") | time_ok >= hms::as_hms("19:00:00")) %>%
  mutate(daynight = as_date(ifelse(time_ok <= hms::as_hms("06:00:00"), date_ok-1, date_ok)),
         h = as.numeric(str_sub(time_ok, start = 1, end = 2)),
         m = as.numeric(str_sub(time_ok, start = 4, end = 5)),
         s = as.numeric(str_sub(time_ok, start = 7, end = 8)),
         time_V2 = ifelse(daynight == date_ok,
                          as.numeric(h*3600+m*60+s),
                          as.numeric((h+24)*3600+m*60+s)),
         nsb_classif =  case_when(
            nsb > 21.7 ~ "21.7",nsb > 21.5 ~ "21.5",nsb > 21.4 ~ "21.4",nsb > 21.3 ~ "21.3",nsb > 21.0 ~ "21.0",
            nsb > 20.4 ~ "20.4",nsb > 19.1 ~ "19.1",nsb > 18.7 ~ "18.7",nsb > 18.0 ~ "18.0",nsb > 0 ~ "16.8"
         ))



brk_time <- c(seq(from = 68400, to = 107000, by = 3600))
label_time <- c("19:00","20:00","21:00","22:00","23:00","00:00",
                "01:00","02:00","03:00","04:00","05:00")

# brk_color <- c("16.8","18.0","18.7","19.1","20.4","21.0","21.3","21.4","21.5","21.7")
# label_color <- c("#960000", "#C80000", "#FF0000", "#FF3503", "#FF7C80", "#FFFF00", "#00CC00", "#01FFFF", "#0033CC", "#7F7F7F")
# 
# brk_color <- c("16.8","18.0","18.7","19.1","21.0")
# label_color <- c("#960000", "#00CC00", "#01FFFF", "#0033CC", "#000000")

ggplot(DATA_03_27, aes(y= time_V2, x = daynight ,color =nsb))+
  geom_point(alpha = 0.2, size =3)+
  
  scale_color_viridis(direction = -1,discrete = FALSE, option="magma")+
  scale_y_continuous(breaks = brk_time, labels = label_time, name = "Heure")+
  scale_x_date(breaks = c(seq(from = as.Date("2020-04-01"), to = as.Date("2020-04-30"), by = "days")), 
               date_labels = "%d")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))

  


    