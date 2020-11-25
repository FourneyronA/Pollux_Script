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

# requete sur la base de donnée
Donnee_NSB <- dbGetQuery(DB_pol_lum, "SELECT date, time, nsb
                                      FROM donnee_capteur") 

# remise en forme des données temporelle pour améliorer les rendu visuel
Donnee_exploitable <- Donnee_NSB %>%
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
                          as.numeric((h+24)*3600+m*60+s)))


# . -------------------------------------------------------------------------- =============
# 4 - Visualisation des données  ====
# . -------------------------------------------------------------------------- =============

brk_time <- c(seq(from = 68400, to = 107000, by = 3600))
label_time <- c("19:00","20:00","21:00","22:00","23:00","00:00",
                "01:00","02:00","03:00","04:00","05:00")

# Visualisation du NSB sur le mois d'avril 
Donnee_exploitable %>%
  ggplot(aes(y= time_V2, x = daynight ,color =nsb))+
  geom_point(alpha = 0.2, size =3)+
  scale_color_viridis(direction = -1,discrete = FALSE, option="magma")+
  scale_y_continuous(breaks = brk_time, labels = label_time)+
  scale_x_date(breaks = c(seq(from = as.Date("2020-04-01"), to = as.Date("2020-04-30"), by = "days")),date_labels = "%d")+
  labs(title = "Analyse de visibilité du ciel proche de Tramoyes", y = "Heure", x = "jour du mois d'Avril 2020")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))



# Visualisation du NSB sur une journée 

Donnee_exploitable %>%
  filter(daynight >= as.Date("2020-04-04") & daynight <= as.Date("2020-04-04"))%>%
  ggplot(aes(y=nsb , x = time_V2 ,color = nsb))+
  geom_line(alpha = 0.8, lwd = 1)+
  scale_color_viridis(direction = -1,discrete = FALSE, option="magma")+
  scale_x_continuous(breaks = brk_time, labels = label_time, name = "Heure", limits = c((68400+1800),(107000-1800)))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))


# comparatif du NSB sur quelques jours

Donnee_exploitable %>%
  filter(daynight >= as.Date("2020-04-03") & daynight <= as.Date("2020-04-06"))%>%
  ggplot(aes(y=nsb , x = time_V2 ,color = as.character(daynight)))+
  geom_line(alpha = 0.8, lwd = 1)+
  scale_x_continuous(breaks = brk_time, labels = label_time, name = "Heure", limits = c((68400+1800),(107000-1800)))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))



    