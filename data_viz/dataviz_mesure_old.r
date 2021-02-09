# . -------------------------------------------------------------------------- =============
# 0 - DataVisualisation des donnees issues des capteurs de mesures ====
# . -------------------------------------------------------------------------- =============

# Le script a pour objectif de lire les donnees fournirs par les capteurs de mesures et de les enregistrer dans la BDD


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
library(gganimate)
library(ggmap)
# . -------------------------------------------------------------------------- =============
# 2 - Connexion BDD postGIS ====
# . -------------------------------------------------------------------------- =============

## Supressions de toutes les connexions pr?c?dentes
lapply(dbListConnections(drv = dbDriver("PostgreSQL")),
       function(x) {dbDisconnect(conn = x)})

# type de connexion PostgreSQL
drv <- dbDriver("PostgreSQL")

# Creation de la connexion
source("C:/Users/fa101525/Desktop/GitHub/connect_bdd.R")


# . -------------------------------------------------------------------------- =============
# 3 - Lecture des donnees  ====
# . -------------------------------------------------------------------------- =============

# requete sur la base de donnee

Donnee_NSB <- st_read(DB_pol_lum, query = "SELECT dc.site_mesure, dc.date, dc.time, sc.geometry, sc.lieu_dit, dc.nsb 
                                      FROM donnee_capteur as dc
                                      INNER JOIN station_capteur as sc 
                                      ON dc.site_mesure = sc.site_mesure_id") 


Donnee_avril <- st_read(DB_pol_lum, query =  "SELECT SUBSTR(dc.date,6,2) as mois,
                                            SUBSTR(dc.date,9,2) as jours,
                                            cast(SUBSTR(dc.time,1,2) as integer) as heure,
                                            cast((CONCAT('\"20', SUBSTR(dc.date,3,2),'-',SUBSTR(dc.date,6,2),'-',SUBSTR(dc.date,9,2),'\"')) as date) as date_ok,
                                            cast(dc.time as time) as time_ok,
                                            dc.site_mesure, dc.nsb, sc.geometry ,
                                            st_srid(sc.geometry)
                                            FROM donnee_capteur as dc, station_capteur as sc 
                                            WHERE sc.site_mesure_id = dc.site_mesure 
                                            AND SUBSTR(dc.date,6,2) = '04'
                                            AND SUBSTR(dc.date,9,2) = '08'")

                                            #AND (cast(SUBSTR(dc.time,1,2) as integer) < 8 OR cast(SUBSTR(dc.time,1,2) as integer) > 19 )

Donnee_avril2 <- Donnee_avril %>%
                  mutate( date_time_full =  ymd_hms(paste(date_ok,time_ok,"UTC")),
                          min = minute(time_ok))  %>%
                  group_by(site_mesure, heure, min) %>%
                  summarise( max_nsb = max(nsb), min_nsb = min(nsb), moy_nsb = mean(nsb)) %>%
                  mutate( time_hour_min = hms(paste(heure,":",min,":","00 UTC", sep ="")))

Donnee_avril3 <- Donnee_avril2 %>% 
                  mutate( time_HMS = paste( ifelse( length(heure) <2,heure,paste("0",heure,sep ="")),":",
                                            ifelse( length(min) <2,min,paste("0",min,sep ="")),":","00 UTC", sep =""),
                          time_hms2 =  ymd_hms(paste("2018-04-08",time_HMS) ))

              
st_write(Donnee_avril3, "C:/Users/fa101525/Desktop/Projet_Pollux/DATA/DONNEE_CARTO/donnee_avril6.shp")


length(unique(Donnee_NSB$site_mesure))

# remise en forme des donnees temporelle pour ameliorer les rendu visuel
Donnee_exploitable <- Donnee_NSB %>%
  mutate(date_ok = as_date(paste("2020/",str_sub(date, start = 6, end = 10),sep = "")),
         time_ok = hms::as_hms(time)) %>%
  filter(date_ok >= as.Date("2020-04-01") & date_ok <= as.Date("2020-04-30"))%>%
  filter(time_ok <= hms::as_hms("06:00:00") | time_ok >= hms::as_hms("19:00:00")) %>%
  mutate(daynight = as_date(ifelse(time_ok <= hms::as_hms("06:00:00"), date_ok-1, date_ok)),
         site_mesure = paste(site_mesure, lieu_dit),
         h = as.numeric(str_sub(time_ok, start = 1, end = 2)),
         m = as.numeric(str_sub(time_ok, start = 4, end = 5)),
         s = as.numeric(str_sub(time_ok, start = 7, end = 8)),
         time_V2 = ifelse(daynight == date_ok,
                          as.numeric(h*3600+m*60+s),
                          as.numeric((h+24)*3600+m*60+s)))


# . -------------------------------------------------------------------------- =============
# 4 - Visualisation des donnees  ====
# . -------------------------------------------------------------------------- =============

brk_time <- c(seq(from = 68400, to = 107000, by = 3600))
label_time <- c("19:00","20:00","21:00","22:00","23:00","00:00",
                "01:00","02:00","03:00","04:00","05:00")


# Visualisation du NSB sur une journee 

# sur un site de mesure
Donnee_exploitable %>%
  filter(daynight >= as.Date("2020-04-04") & daynight <= as.Date("2020-04-04"))%>%
  filter(site_mesure == "site 11")%>%
  ggplot(aes(y=nsb , x = time_V2 ,color = nsb))+
  geom_line(alpha = 0.8, lwd = 1)+
  scale_color_viridis(direction = -1,discrete = FALSE, option="magma")+
  scale_x_continuous(breaks = brk_time, labels = label_time, name = "Heure", limits = c((68400+1800),(107000-1800)))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))

# sur l'ensemble des sites de mesure 
Donnee_exploitable %>%
  filter(daynight >= as.Date("2020-04-04") & daynight <= as.Date("2020-04-04"))%>%
  ggplot(aes(y=nsb , x = time_V2 ,color = site_mesure))+
  geom_line(alpha = 0.8, lwd = 1)+
  scale_x_continuous(breaks = brk_time, labels = label_time, name = "Heure", limits = c((68400+1800),(107000-1800)))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))


# comparatif du NSB sur quelques jours

Donnee_exploitable %>%
  filter(daynight >= as.Date("2020-04-03") & daynight <= as.Date("2020-04-06"))%>%
  ggplot(aes(y=nsb , x = time_V2 ,color = as.character(daynight)))+
  geom_line(alpha = 0.8, lwd = 1)+
  scale_x_continuous(breaks = brk_time, labels = label_time, name = "Heure", limits = c((68400+1800),(107000-1800)))+
  facet_wrap(vars(site_mesure))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))

# . -------------------------------------------------------------------------- =============
# 4 - cartographie des donnees  ====
# . -------------------------------------------------------------------------- =============

# visualisation interractive simple des donnees sur une journee pour aprehender les donnees 


donnee_journee <- Donnee_exploitable %>%
  filter(daynight >= as.Date("2020-04-04") & daynight <= as.Date("2020-04-04"))

mapview(donnee_journee, zcol = "nsb", 
        col.regions = rev(magma(9)),
        alpha = 0.2,
        legend = TRUE)  # verification du resultat 

# visualisation interractive simple des donnees sur une journee pour aprehender les donnees 

test <- Donnee_exploitable %>%
  filter(daynight >= as.Date("2020-04-04") & daynight <= as.Date("2020-04-04")) %>%
  filter(time_V2 >= 68400 & time_V2 <= (68400+3600) )
  
  
ggplot(data = test) +
    stat_sf_coordinates(aes( colour = nsb), size = 8)+
    theme_void()+
    scale_color_viridis(direction = -1,discrete = FALSE, option="magma") +
    labs(title = "Visibilité lumineuse: 19h{closest_state}") +
    transition_states(m)



test2 <- Donnee_NSB %>%
  mutate(date_ok = as_date(paste("2020/",str_sub(date, start = 6, end = 10),sep = "")),
         time_ok = hms::as_hms(time)) %>%
  filter(date_ok >= as.Date("2020-04-01") & date_ok <= as.Date("2020-04-30"))%>%
  mutate( h = as.numeric(str_sub(time_ok, start = 1, end = 2))) %>%
  group_by(site_mesure,date_ok, h) %>%
  summarise(moy_nsb = mean(nsb), max_nsb = max(nsb), min_nsb = min(nsb)) #%>%
  #mutate(h = ifelse(h>18,h,(h+24)))

test3 <- test2 %>%
  filter(date_ok >= as.Date("2020-04-01") & date_ok <= as.Date("2020-04-05"))%>%
  mutate(datetime_ok = as_datetime(paste( date_ok," ", ifelse(h<10, paste("0",h,sep=""),h) ,":00:00 UTC", sep = ""), tz = "UTC") ) %>%
  st_transform(crs = 4326)
  
bbox(test3$geometry)

ggplot(data = test3) +
  stat_sf_coordinates(aes( colour = max_nsb), size = 8)+
  jpnMap <- get_stamenmap(bbox = c(left = min(4.859357), bottom = min(45.87786), right = max(5.092782), top = max(45.94923)),maptype = "toner-lite", zoom = 4)+ 
  theme_void()+
  scale_color_viridis(direction = -1,discrete = FALSE, option="magma")
  labs(title = "Visibilité lumineuse: {closest_state}") +
  transition_states(datetime_ok)
