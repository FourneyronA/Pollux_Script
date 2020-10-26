# . -------------------------------------------------------------------------- =============
# 0 - Préambule et explication ====
# . -------------------------------------------------------------------------- =============

    # L'objectif de ce script est de transformer des fichiers raster qui se décompose en 3 fichiers (grd, hdr et tif)
    # ce format particulier n'est pas compris pas les outils SIG classique. 
    # le script permet de lire l'ensemble des fichiers pour ensemble les informations afin d'obtenir un fichier GEOtiff
    
    # Pour réaliser cette operation le script à besoin d'avoir plusieurs informations 
      # 1-  dossier qui contient l'ensemble des fichiers 
      # 2-  le type de projection utilisé
    
    # Grâce à ses informations nous allons parcourir l'ensemble du dossier, lire pour chaque fichier le grd et le tif associé
    # assembler les informations sur la géolocalisation du grd pour le combiner avec la matrice contenue dans le tif


# . -------------------------------------------------------------------------- =============
# 1 - Variable à définir ====
# . -------------------------------------------------------------------------- =============


## VARIABLE A METTRE A JOURS : 

### dossier où sont localiser les fichiers tiff et grd
chemin_dossier <- "C:/Users/fourn/Documents/Brain/CNRS/Projet Pollution lumineuse/DATA/MNS_Ain/dpsg2018-06-00021"

### Nouveau dossier qui contiendra les Geotiffs
nouveau_dossier <- "MNE_AIN_OK_VF"

### Sytème de projection des fichier tiff 
epsg <- "+init=epsg:2154"

### Dossier de réference du projet 
BASE_WORKING <- getwd()
#setwd("C:/Users/fourn/Documents/Brain/CNRS/Projet Pollution lumineuse/Projet/Traitement_R")


# . -------------------------------------------------------------------------- =============
# 2 - Chargement des packages ====
# . -------------------------------------------------------------------------- =============

pkgs <-  c("dplyr","stringr", "rgdal", "raster","sf", "tidyr", "tiff")


if (length(setdiff(pkgs, rownames(installed.packages()))) > 0) {
  # installation des packages nécéssaires 
  install.packages(setdiff(pkgs, rownames(installed.packages())))  
} 

  # chargement des packages 
  lapply(pkgs, library, character.only = TRUE)
  rm(pkgs)

# . -------------------------------------------------------------------------- =============
# 3 - Chargement des fonction ====
# . -------------------------------------------------------------------------- =============

## fonction permettant de lancer la conversion de fichier 

# lecture du fichier GRF pour obtenir le positionnement de l'image tif
LECTURE_GRF <- function(nom_fichier){
  
  # lecture du fichier 
  grf <- read.delim(nom_fichier, header=FALSE, sep = ":")
  
  # class(grf)
  # head(grf)
  
  #récuperation des Xmin, Ymin, Xmax, Ymax
  
  Xmin <- grf %>% # solicitation du tableau contenant l'ensemble des informations 
    filter(V1 == "X minimum ")  %>% # filtre sur le X min 
    dplyr::select(V2) %>% # selection uniquement de la valeur 
    as.numeric() # conversion de la valeur en numéric
  
  Xmax <- grf %>%
    filter(V1 == "X maximum ") %>%
    dplyr::select(V2) %>%
    as.numeric()
  
  Ymin <- grf %>%
    filter(V1 == "Y minimum ") %>%
    dplyr::select(V2) %>%
    as.numeric()
  
  Ymax <- grf %>%
    filter(V1 == "Y maximum ") %>%
    dplyr::select(V2) %>%
    as.numeric()
  
  # print(c(Xmin, Xmax, Ymin, Ymax))
  
  # renvoie des valeurs
  return(c(Xmin,Xmax,Ymin,Ymax)) 
  
}

# lecture de l'image tif et conversion puis enregistrement en GEOtiff à partir des données GRF
TIFF_TO_GEOTIFF <- function(chemin_dossier,nom_fichier,nouveau_dossier, epsg, XY){
  
  #lecture de l'image tiff dans une matrix
  t = tiff::readTIFF( paste(chemin_dossier,"/",nom_fichier, sep = ""))
 
   # conversion de la matrix en fichier raster 
  T2 <- raster(t)
  
   # Positionnement du raster dans l'espace 
  extent(T2) <- c(XY[1], XY[2], XY[3], XY[4])
    
   # Renseignement du type de projection utilisé
  projection(T2) <- CRS("+init=epsg:2154")
  
   # afficahnge de l'image 
  # plot(T2)
   
   # affichage des données min et max contenue dans le tiff
  # print(paste("min :",min(T2@data@values)))
  # print(paste("max :",max(T2@data@values)))
  
   # déplacement dans le nouveau dossier 
  setwd(nouveau_dossier)
   # enregistrement du fichier Geotiff dans le dossier spécifique
  rf <- writeRaster(T2, filename= nom_fichier, format="GTiff", overwrite=TRUE)
}


# . -------------------------------------------------------------------------- =============
# 4 - Lancement des conversions ====
# . -------------------------------------------------------------------------- =============

# Création du dossier 
dir.create(nouveau_dossier)

# Nombre de fichier dans le fichier de donnée à traiter
nb_fichier <- dir(path = chemin_dossier)
# Nombre totale de fichier présent 
length(nb_fichier)

for (i in 1:(length(nb_fichier)/3) ) { #boucle pour traiter chaque groupe de fichier 
  
  pointer_grd = (1 + (3 * ( i - 1))) # numero du fichier qui correspond au grd
  pointer_tif = 3*i # numero du fichier qui correspond au tiff
  # nb_fichier[pointer_grd] # permet d'affichier le nom du fichier avec son extention
  
  # recuperation des xim, xmax, ymin, ymax
  XY <- LECTURE_GRF(paste(chemin_dossier,"/",nb_fichier[pointer_grd], sep = ""))
  
  # Enregistrement du fichier tiff géoréférencé
  TIFF_TO_GEOTIFF(chemin_dossier,nb_fichier[pointer_tif],nouveau_dossier,epsg,XY)
  
  # remise dans le dossier initiale
  setwd(BASE_WORKING)
  
  # Affichage du % de travail effectué 
  print(paste("Fichier réalisé :", round((i/(length(nb_fichier)/3)*100),2), "%" ))
  
}