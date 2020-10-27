# . -------------------------------------------------------------------------- =============
# 0 - Préambule et explication ====
# . -------------------------------------------------------------------------- =============

# . -------------------------------------------------------------------------- =============
# 1 - Librairie ====
# . -------------------------------------------------------------------------- =============

library(sf)
library(raster)
library(rasterVis)
library(viridis)

library(ggplot2)
library(tidyverse)
library(cowplot)


# . -------------------------------------------------------------------------- =============
# 2 - Lecture des fichiers ====
# . -------------------------------------------------------------------------- =============

# Lecture des fichiers brute SHP et GeoTif ====

epsg <- ("+init=epsg:2154")

# chemin de localisation des données
### DONNEES MNT 
## chemin_dossier = "C:/Users/fa101525/Desktop/Projet_Pollux/Projet/Traitement_R/mnt_test" ## fichier donnée test
chemin_dossier = "C:/Users/fa101525/Desktop/Projet_Pollux/DATA/MNT_AIN" ## fichier reel donnée
nb_fichier_MNT <- dir(path = chemin_dossier)
length(nb_fichier_MNT)

setwd("C:/Users/fa101525/Desktop/Projet_Pollux/DATA/MNT_AIN")  ## fichier reel donnée
raster_image <- raster(nb_fichier_MNT[300]) # test sur une seule image raster (parmis les 2450...)

# . -------------------------------------------------------------------------- =============
# 3 - Traitements des données raster ====
# . -------------------------------------------------------------------------- =============

### Changement de résolution ====

# visualisation simple des raster après changement de résolution 
#[1] 0.25 x 0.25
res(raster_image)
levelplot(raster_image, layers = 1, margin = list(FUN = 'median'), contour=FALSE)


#[1] 1 X 1
r.aggregate_1 <- aggregate(raster_image, fact=4)
res(r.aggregate_1)
levelplot(r.aggregate_1, layers = 1, margin = list(FUN = 'median'), contour=FALSE)

#[1] 2 X 2
r.aggregate_2 <- aggregate(raster_image, fact=8)
res(r.aggregate_2)
levelplot(r.aggregate_2, layers = 1, margin = list(FUN = 'median'), contour=FALSE)


### Analyse du changement de résolution ====

# fonction qui permet d'afficher l'image après le changement de résolution et qui compare par rapport a une résolution de base les différences des données enregistré

raster_to_data <- function(fichier_raster, agregation){
  
  r.aggregate_1 <- aggregate(fichier_raster, fact=2)
  Raster <-  aggregate(fichier_raster, fact=agregation)
  #Raster <- r.aggregate_15
  surface_pix_data <- res(r.aggregate_1)[1] * res(r.aggregate_1)[2]
  total_surf <- r.aggregate_1@ncols * r.aggregate_1@nrows * res(r.aggregate_1)[1] * res(r.aggregate_1)[2]

  data_1_classif <- data.frame(r.aggregate_1@data@values) %>%
    rename(val = 'r.aggregate_1.data.values' ) %>%
    mutate(
      classif =  case_when(
        val > 250 ~ "250",
        val > 245 ~ "245",
        val > 240 ~ "240",
        val > 235 ~ "235",
        val > 230 ~ "230",
        val > 225 ~ "225",
        val > 220 ~ "220",
        val > 215 ~ "215",
        val > 210 ~ "210",
        val > 205 ~ "205",
        val > 200 ~ "200",
        val > 195 ~ "195",
        val > 190 ~ "190",
        val > 185 ~ "185",
        val > 180 ~ "180",
        val > 175 ~ "175",
        val > 170 ~ "170"
      ))
  
  data_1 <- data_1_classif %>%
    group_by(classif) %>%
    summarise( nb_pixel = n() ) %>%
    mutate( sum_surface = nb_pixel * surface_pix_data,
            resolution = paste("A_",res(r.aggregate_1)[1],"x", res(r.aggregate_1)[2], sep =""))

  
  
  surface_pix <- res(Raster)[1] * res(Raster)[2]
  data_classif <- data.frame(Raster@data@values) %>%
    rename(val = 'Raster.data.values' ) %>%
    mutate(
      classif =  case_when(
        val > 250 ~ "250",
        val > 245 ~ "245",
        val > 240 ~ "240",
        val > 235 ~ "235",
        val > 230 ~ "230",
        val > 225 ~ "225",
        val > 220 ~ "220",
        val > 215 ~ "215",
        val > 210 ~ "210",
        val > 205 ~ "205",
        val > 200 ~ "200",
        val > 195 ~ "195",
        val > 190 ~ "190",
        val > 185 ~ "185",
        val > 180 ~ "180",
        val > 175 ~ "175",
        val > 170 ~ "170"
      ))
  
  group_classif <- data_classif %>%
    group_by(classif) %>%
    summarise( nb_pixel = n() ) %>%
    mutate( nb_pixel = replace_na(nb_pixel, 0),
            sum_surface = nb_pixel * surface_pix,
            resolution = paste("B_",res(Raster)[1],"x", res(Raster)[2], sep =""))
  

  All_data <- rbind(data_1, group_classif)
  colum_data <- full_join(data_1,group_classif, by = "classif" ) 
  colum_data <- colum_data %>% replace_na(list("sum_surface.y" = 0))
  colum_data$diff_surf <- colum_data[,3] - colum_data[,6]

  surface_diff <- sum(abs(colum_data$diff_surf$sum_surface.x))
  surface_ok <- abs(sum(colum_data[,6])) - surface_diff
  
  
  
  img_raster <-  levelplot(Raster, layers = 1, margin = list(FUN = 'median'), contour=FALSE)
  
  BAR_PLOT <- All_data %>%
    group_by(resolution) %>%
    summarise(total_surface = sum(sum_surface)) %>%
    ggplot(aes(x = resolution, y = total_surface, fill = resolution)) +
    geom_bar(stat = "identity", fill = c("chartreuse3", "chocolate3"))+
    labs(x = "", y = "")+
    coord_flip()+
    theme_minimal() +
    theme(legend.position = "none") 
  
  HISTO <- ggplot(colum_data, aes( x = classif, y = (diff_surf$sum_surface.x)*(-1) )) +
    geom_bar(stat = "identity", position = position_dodge(), fill = "brown4")+
    ggtitle("Différence des superficies incorrect par altitude") +
    labs(y = "surface totale", x = "altitude", fill = "résolution")+
    theme_minimal() +
    theme(legend.position = "none") 
  

  
  
  # Create Data
  data <- data.frame(
    group=c("Surface OK","Surface Incorrect"),
    value=c(surface_ok,surface_diff)
  )
  
  # Compute the position of labels
  data <- data %>% 
    arrange(desc(group)) %>%
    mutate(prop = value / sum(data$value) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  
  # Basic piechart
  PIE_CHAR <- ggplot(data, aes(x="", y=prop, fill=group)) +
    geom_bar(stat="identity", width=1, color="white", fill = c("chartreuse3", "brown4")) +
    coord_polar("y", start=0) +
    theme_void() + 
    ggtitle(paste("surface incorrect : ", round(surface_diff, 2),"m²"))+
    theme(legend.position="none") 
    #+ geom_text(aes(y = ypos, label = group), color = "white", size=3) 
  
  
  
  TG <- plot_grid(BAR_PLOT, PIE_CHAR)
  TG2 <- plot_grid(HISTO, TG, nrow = 2)
  plot_grid(img_raster, TG2)
}



# . -------------------------------------------------------------------------- =============
# 4 - Visualisation des données raster ====
# . -------------------------------------------------------------------------- =============

raster_to_data(raster_image, 4) # resolution 1x1 mètre
raster_to_data(raster_image, 8) # resolution 2x2 mètre
raster_to_data(raster_image, 20) # resolution 5x5 mètre
raster_to_data(raster_image, 40) # resolution 10x10 mètre
raster_to_data(raster_image, 60) # resolution 15x15 mètre
raster_to_data(raster_image, 80) # resolution 20x20 mètre
raster_to_data(raster_image, 160) # resolution 40x40 mètre
raster_to_data(raster_image, 320) # resolution 80x80 mètre
raster_to_data(raster_image, 640) # resolution 160x160 mètre


