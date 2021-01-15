#### VERSION TEST =====
# Analyse de visibilité lumineuse


wd = "C:/Users/fa101525/Desktop/TEST_PIXSCAPE/"
name_projet = "Projet1/Projet1.xml"
library(raster)
library(mapview)
library(sf)


setwd(wd)
getwd()

project = name_projet
zeye = 1.7
zdest = -1 

# x = 857555.61
# y = 6533985.95
# x = 859802.00462
# y = 6534532.68499

Analyse_visibilite_point <- function(x, y, zeye, zdest){
  
  resfile="result_raster_test.tif"
  pixscape = "java -jar pixscape-1.2.jar"
  
  if(file.exists("Projet1/result_raster_test.tif") & file.exists("Projet1/result_raster_test.tfw") ){
    file.remove("Projet1/result_test.tif")
    file.remove("Projet1/result_test.tfw")
  }
  
  command1 = paste0("cd ", wd) 
  command2 = paste(pixscape, "--project ", paste0(wd, project))
  command4 = paste(command2, "-zeye", zeye, "-zdest", zdest, "--viewshed", x, y,
                   paste0("resfile=",resfile))
  
  system(command1)
  system(command4)
  
  #system("java -jar pixscape-1.2.jar --project C:/Users/fa101525/Desktop/Projet_Pollux/Pixscape/Projet_Pollux/Projet1/Projet1.xml -zeye 1 -zdest 1 --viewshed 858736.458 6534756.667 resfile=bassin1.tif")
  # x = 859034.875
  # y = 6535491.625
  
  # Limite de la zone d'etude 
  Zone_etude <- st_read('Projet1/Saint_Croix_limite_com.shp')
  
  # Raster des points visibles
  temp = raster("Projet1/result_raster_test.tif", proj4string = CRS("+init=epsg:2154"))
  crs(temp) <- CRS("+init=epsg:2154")
  
  # Raster des luminosites
  lumi = raster("Projet1/NASA_VIIRSD_2.tif", proj4string = CRS("+init=epsg:2154"))
  crs(lumi) <- CRS("+init=epsg:2154")
  
  # raster des distances 
  R_dist <- distanceFromPoints(temp, c(x,y)) 
  crs(R_dist) <- CRS("+init=epsg:2154")
  
  # mapview(temp) + mapview(temp3) + mapview(R_dist)+ mapview(lumi)
  temp3 <- temp >= 1
  
  lumi_reso_fine <- lumi %>%
    crop(temp3) %>%
    setExtent(temp3) #%>%
  #aggregate(fact=c(res(temp)[1], res(temp)[2]))
  
  test_lumi <- st_read('Projet1/lumi.shp')
  
  # Make a raster of the wetlands:
  coastline.r <- rasterize(test_lumi, temp3)
  
  
  Visi_Dist_ZE <- temp3 * R_dist
  #plot(Visi_Dist_ZE)
  
  Visi_lumi_ZE <- temp3 * (coastline.r *100)
  #plot(Visi_lumi_ZE)
  
  visibilite <- Visi_lumi_ZE * ( 1 / (R_dist^(2.5)) )
  tgfdp <- getValues(visibilite)
  tgfdp2 <- na.omit(tgfdp) 
  tgfdp3 <- tgfdp2[tgfdp2>0.5]
  
  TEST_valeur_sum <- sum(tgfdp3)
  TEST_valeur_moy <- mean(tgfdp3)
  TEST_valeur_max <- max(tgfdp3)
  
  TEST_valeur_sum
  TEST_valeur_moy
  TEST_valeur_max
  
  mapview(visibilite)
  
  data_lumi <- data.frame(Somme = TEST_valeur_sum, Moyenne = TEST_valeur_moy, Maximum = TEST_valeur_max, x = x, y = y)
  
  file.remove("Projet1/result_raster_test.tif")
  file.remove("Projet1/result_raster_test.tfw")
  
  return(data_lumi)
}


zone_point <- st_read('Projet1/point_first_try.shp')
data_point <- data.frame()

message("====== loading =====")
load <- 0 

for(i in 6:nrow(zone_point)){
  x = st_coordinates(zone_point$geometry[i])[1]
  y = st_coordinates(zone_point$geometry[i])[2]
  
  data_ok <- Analyse_visibilite_point(x, y, zeye, zdest)
  data_point <- data_point %>%
    rbind(data_ok)
  
  real <- round((((event -(nb_event-event)) /nb_event)*100),-1)
  if(real > load){
    message("==", appendLF = FALSE)
    load <- real
  }
}

data_final <- data_point %>%
  st_as_sf(coords = c("x", "y"), crs = 2154)

mapview(data_final, zcol = c("Somme", "Moyenne", "Maximum")) + mapview(test_lumi, zcol ="lumi")


ggplot(data_final, aes(x = Somme))+
  geom_histogram()

ggplot(data_final, aes(x = Moyenne))+
  geom_histogram()

ggplot(data_final, aes(x = Maximum))+
  geom_histogram()
