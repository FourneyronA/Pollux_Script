#### VERSION TEST =====

# . -------------------------------------------------------------------------- =============
# 0 - Analyse de visibilite lumineuse ====
# . -------------------------------------------------------------------------- =============

# Le script a pour objectif de convertir les donnees meteo france disponible en CSV dans une BDD spatiale. 
# ATTENTION il est imperatif d'adapter les parties suivantes : 
# 2 - Variable et parametre : (emplacement des fichiers)
# 3 - Connexion BDD postGIS : (connexion a votre base de donnee)

# Nous avons deux type de donnees : 
# - Les stations de mesures georeferencer : https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/postesSynop.csv
# - Les mesures des stations : https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=90&id_rubrique=32
# - Les deplacements des nuages georeferencer : https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=130&id_rubrique=51


# . -------------------------------------------------------------------------- =============
# 1 - Chargement des packages ====
# . -------------------------------------------------------------------------- =============

pkgs <-  c("dplyr","mapview","ggplot2", "raster","sf") # "rgdal", "tidyr", "tiff"

if (length(setdiff(pkgs, rownames(installed.packages()))) > 0) {
  # installation des packages 
  install.packages(setdiff(pkgs, rownames(installed.packages())))  
} 
# chargement des packages 
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

# . -------------------------------------------------------------------------- =============
# 2 - initialisation des variables ====
# .-------------------------------------------------------------------------- =============

# Emplacement des fichiers ====
wd = "C:/Users/fa101525/Desktop/TEST_PIXSCAPE/"
name_projet = "Projet1/Projet1.xml"

setwd(wd)
getwd()

# hauteur des yeux ====
zeye = 1.7
# hauteur de visualisation ====
zdest = 1 

# x = 859802.00462
# y = 6534532.68499

# . -------------------------------------------------------------------------- =============
# 3 - Modelisation pollution lumineuse  ====
# . -------------------------------------------------------------------------- =============

### 3.0 - Lumiere issues des images satellites  ====

# Creation de raster LUMI_OK
# test_lumi <- st_read('Projet1/lumi.shp')
# luminosite <- rasterize(test_lumi, temp3)
# class(luminosite)
# rf <- writeRaster(luminosite, filename= 'Projet1/lumi_ok', format="GTiff", overwrite=TRUE)

Raster_lumi = raster("Projet1/DATA_CONTEXT/lumi_ok.tif", proj4string = CRS("+init=epsg:2154"))
crs(Raster_lumi) <- CRS("+init=epsg:2154")

### 3.1 - Lumiere issues des routes  ====


# Lectures des lignes de routes avec leurs TMJA
  Route_Ain <- st_read('Projet1/DATA_CONTEXT/RD_trafic.shp') 
# Lectures des limites communales
  Limite_SC <- st_read('Projet1/DATA_CONTEXT/Saint_Croix_limite_com.shp')
# Decouper les route de la zone d'etudes 
  Route_SC <- st_crop(Route_Ain,Limite_SC)
# Calcule des lumens issues des routes TMJA x XXXX
  Route_SC_lumi <- Route_SC %>%
    mutate(lumen = case_when(trafic_mja > 10000 ~ 1500, 
                             trafic_mja > 5000 ~ 500, 
                             trafic_mja > 1000 ~ 250, 
                             TRUE ~ 0)) 
# generation d'un raster (Nombre de lumens par carree)
  Raster_lumi_Route <- rasterize(Route_SC_lumi, Raster_lumi,field = 'lumen', fun=max) 
  
  # mapview(Raster_lumi_Route)

### 3.2 - Lumiere issues des batiments ====

# Lectures des batiments avec leurs populations
  Bati_Ain <- st_read('Projet1/DATA_CONTEXT/BATIMENT.shp') 
  # Decouper les route de la zone d'etudes 
  Bati_SC <- st_crop(Bati_Ain,Limite_SC)
# Calcule des lumens issues des batiments POP x XXXX
  Bati_SC_lumi <- Bati_SC %>%
    mutate(lumen = case_when(USAGE1 == "Commercial et services" ~ 250, 
                             USAGE1 == "Résidentiel" ~ 150, 
                             TRUE ~ 0))  
# generation d'un raster (Nombre de lumens par carree)
  Raster_lumi_Bati <- rasterize(Bati_SC_lumi, Raster_lumi,field = 'lumen', fun='sum') 
  
  # mapview(Bati_SC_lumi)+
  #   mapview(Raster_lumi_Bati)+
  #   mapview(Raster_lumi)
  


### 3.3 - Lumiere issues des lampadaires ====

# Lectures des lignes de routes 
  Route_SC
# Lectures des zones urbaines 
  Zone_urbaine_Ain <- st_read('Projet1/DATA_CONTEXT/ZONE_D_HABITATION.shp') 
# Découpages des zones urbaines dans le village
  Zone_urbaine_SC <- st_crop(Zone_urbaine_Ain,Limite_SC)
  
# Selection des routes dans les zones urbaines 
  Route_Zone_urbaine_SC <- st_intersection(st_union(Route_SC),st_union(Zone_urbaine_SC))
  numOfPoints  <- round(as.numeric(st_length(Route_Zone_urbaine_SC) / 25),0)
  # Generation des points de lampadaires tout les 25m a 1250lumen a une hauteur de 7m
  Lampadaire_in_ZU_SC<- Route_Zone_urbaine_SC %>%
    st_cast("LINESTRING") %>%
    st_sample(size = numOfPoints, type = "regular")%>%
    st_cast("POINT") %>%
    st_as_sf() %>%
    mutate(lumen = 1250, hauteur = 7)

# Selection des routes en dehors des zones urbaines
  Route_OUT_Zone_urbaine_SC <- st_difference(st_union(Route_SC),st_union(Zone_urbaine_SC))
  numOfPoints  <-   round(as.numeric(st_length(Route_OUT_Zone_urbaine_SC) / 50),0)
  # Generation des points de lampadaires tout les 50m a 700lumen a une hauteur de 10m
  Lampadaire_out_ZU_SC<- Route_OUT_Zone_urbaine_SC %>%
    st_cast("LINESTRING") %>%
    st_sample(size = numOfPoints, type = "regular")%>%
    st_cast("POINT") %>%
    st_as_sf() %>%
    mutate(lumen = 700, hauteur = 10)

# groupement des points lampadaires 
  All_Lampadaire <- Lampadaire_out_ZU_SC %>%
    rbind(Lampadaire_in_ZU_SC)
  
  st_write(All_Lampadaire, "lampadaire.shp")

# generation d'un raster (Nombre de lumens par carree)
  
Raster_lumi_Lampadaire <- rasterize(All_Lampadaire, Raster_lumi,field = 'lumen', fun='max') 

  # mapview(Raster_lumi_Route) + 
  #   mapview(All_Lampadaire) +
  #   mapview(Zone_urbaine_SC)+
  #   mapview(Lampadaire_out_ZU_SC)+
  #   mapview(Lampadaire_in_ZU_SC)

# . -------------------------------------------------------------------------- =============
# 4 - Analyse de visibilite lumineuse via PIXSCAPE ====
# . -------------------------------------------------------------------------- =============

Analyse_visibilite_point <- function(xy, zeye, zdest, Raster_lumi, project){
  
  x = xy[1]
  y = xy[2]
  
  resfile="result_raster_test.tif"
  pixscape = "java -jar pixscape-1.2.jar"
  
  if(file.exists("Projet1/result_raster_test.tif") & file.exists("Projet1/result_raster_test.tfw") ){
    file.remove("Projet1/result_raster_test.tif")
    file.remove("Projet1/result_raster_test.tfw")
  }
  
  command1 = paste0("cd ", wd) 
  command2 = paste(pixscape, "--project ", paste0(wd, project))
  command4 = paste(command2, "-zeye", zeye, "-zdest", zdest, "--viewshed", x, y,
                   paste0("resfile=",resfile))
  
  system(command1)
  system(command4)
  
  # Raster des points visibles
  temp = raster("Projet1/result_raster_test.tif", proj4string = CRS("+init=epsg:2154"))
  crs(temp) <- CRS("+init=epsg:2154")
  # temp3 <- temp >= 1
  # res(temp3)
  # extent(temp3)
  # raster des distances 
  R_dist <- distanceFromPoints(temp, c(x,y)) 
  crs(R_dist) <- CRS("+init=epsg:2154")
  
  #Visi_Dist_ZE <- mask(R_dist, temp, maskvalue = 0)
  # Visi_Dist_ZE <- temp3 * R_dist
  #plot(Visi_Dist_ZE)
  
  All_raster_lumi <- stack(Raster_lumi,  Raster_lumi_Bati, Raster_lumi_Route, Raster_lumi_Lampadaire) 
  Visi_lumi_ZE <- mask(All_raster_lumi, temp, maskvalue = 0) 
  # Visi_lumi_ZE <- temp3 * (Raster_lumi)
  #plot(Visi_lumi_ZE)
  
  visibilite <- overlay(Visi_lumi_ZE, R_dist, fun=function(r1, r2){return((r1)/r2)})
  # visibilite1 <- overlay(Visi_lumi_ZE[[1]], R_dist, fun=function(r1, r2){return((r1*683)/r2)})
  # visibilite3 <- overlay(Visi_lumi_ZE[[3]], R_dist, fun=function(r1, r2){return((r1)/r2)})
  # visibilite4 <- overlay(Visi_lumi_ZE[[4]], R_dist, fun=function(r1, r2){return((r1)/r2)})
  #visibilite <- overlay(Visi_lumi_ZE, R_dist, fun=function(r1, r2){return((r1*683)/(r2^(0.5)))})
  
  data_visi <- data.frame(visi = getValues(visibilite[[1]]),
                          visi_bati = getValues(visibilite[[2]]),
                          visi_route = getValues(visibilite[[3]]),
                          visi_lampe = getValues(visibilite[[4]])) %>%
    filter( !is.na(visi) & visi > 0)
  

  data_lumi <- data.frame(lum_som =sum(data_visi$visi),
                          lum_moy =mean(data_visi$visi),
                          lum_max =max(data_visi$visi),
                          nb_surf =nrow(data_visi),
                          lum_bati=sum(data_visi$visi_bati,na.rm=TRUE),
                          nb_bati =length(na.omit(data_visi$visi_bati)),
                          lum_route =sum(data_visi$visi_route,na.rm=TRUE),
                          nb_route =length(na.omit(data_visi$visi_route)),
                          lum_lampe =sum(data_visi$visi_lampe,na.rm=TRUE),
                          nb_lampe =length(na.omit(data_visi$visi_lampe)),
                          x = x,
                          y = y)
  # List_value = c(data_visi$visi),
  
  file.remove("Projet1/result_raster_test.tif")
  file.remove("Projet1/result_raster_test.tfw")
  
  return(data_lumi)
}


# Chargement de la grilles de resultats ====
zone_point <- st_read('Projet1/DATA_ANALYSE/point_100m.shp')  #point_100m.shp
data_point <- data.frame()


# Lancement de l'analyse de visibilité ====
load <- 0 
long <- nrow(zone_point)
long
Start <- Sys.time() 

for(i in 1:long){ #nrow(zone_point)

  data_ok <- Analyse_visibilite_point(st_coordinates(zone_point$geometry[i]), zeye, zdest,Raster_lumi , name_projet)
  data_point <- data_point %>%
    rbind(data_ok)
  
  real <- round((((i -(long-i)) /long)*100),-1)
  if(real > load){
    print(paste("Traitement realisee : ", real, "%"))
    load <- real
  }
}

End <- Sys.time()  - Start
End



# . -------------------------------------------------------------------------- =============
# 5 - Visualisation des résultats de visibilite lumineuse ====
# . -------------------------------------------------------------------------- =============

# Remise en forme des donnees resultats ====
data_final <- data_point %>%
  st_as_sf(coords = c("x", "y"), crs = 2154) %>%
  mutate(lum_bati = ifelse(is.na(lum_bati), 0, lum_bati ),
         lum_route = ifelse(is.na(lum_route), 0, lum_route ),
         lum_lampe = ifelse(is.na(lum_lampe), 0, lum_lampe ),
         sum_lum_mod = lum_bati + lum_route + lum_lampe)

st_write(data_final, "data_100m.shp")
#   mutate( Moyenne = ifelse(Somme == 0, 0, Moyenne),
#           Maximum = ifelse(Somme == 0, 0, Maximum))

colnames(data_final)
list_col <- c("lum_som", "lum_moy",  "lum_max", "nb_surf","lum_bati", "nb_bati","lum_route","nb_route","lum_lampe", "nb_lampe", "sum_lum_mod")
# Cartographie des interractives des resultats ====
mapview(data_final, zcol = list_col)

mapview(data_final, zcol = "lum_lampe") +  
  mapview(data_final, zcol = "nb_lampe") + 
  mapview(All_Lampadaire)




# Histogramme des differentes variables ====
ggplot(data_final, aes(x = Somme))+
  geom_histogram()

ggplot(data_final, aes(x = Moyenne))+
  geom_histogram()

ggplot(data_final, aes(x = Maximum))+
  geom_histogram()


# Analyses des impacts entre Route/Lampadaire/Bati ====
ggplot(data_final, aes(x = nb_lampe  , y = nb_surf, fill = lum_lampe, col = lum_lampe ))+
  geom_point(pch = 21) +
  theme_minimal()

ggplot(data_final, aes(x = Surface_vis, y = Somme, fill = log(Moyenne), col = log(Moyenne) ))+
  geom_point(pch = 21)