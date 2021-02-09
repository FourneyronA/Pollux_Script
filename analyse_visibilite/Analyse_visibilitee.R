wd = "C:/Users/fa101525/Desktop/Projet_Pollux/Pixscape/Projet_Pollux/"
name_projet = "Projet1/Projet1.xml"
library(raster)
library(mapview)
library(sf)
###### parameters
# create a project from the java application, in the folder of the .jar
# project = project emplacement (.xml)
# zeye = height of the observer
# zdest = height of the observed


#pixscapeR = function(project, zeye, zdest, x, y) {
# return(temp)
# }
  setwd(wd)
  getwd()

  project = name_projet
  zeye = 1.7
  zdest = -1 
  x = 859034.875
  y = 6535491.625
  resfile="result_test.tif"

 
  pixscape = "java -jar pixscape-1.2.jar"
  
  if(file.exists("Projet1/result_test.tif") & file.exists("Projet1/result_test.tfw") ){
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
  x = 859034.875
  y = 6535491.625

  temp1 = raster("Projet1/result_test.tif", proj4string = CRS("+init=epsg:2154"))
  crs(temp1) <- CRS("+init=epsg:2154")
  
  
  
  test <- temp %>%
    rasterToPolygons() %>% #fasterVectorize( vectType='area', grassDir=grassDir) %>%
    st_as_sf()
  
  length(p)
  length(temp)
  30055025 - 729180
  
  lumi = raster("Projet1/NASA_VIIRSD_Sainte_croix.tif", proj4string = CRS("+init=epsg:2154"))
  crs(lumi) <- CRS("+init=epsg:2154")
  mapview(temp) + mapview(lumi)
  #crs(lumi) <- CRS("+init=epsg:2154")
  test_viirs <- mask(lumi, temp)
  
  test_viirs <- extract(lumi, p)
  
  # extract(rDERIVMNT_RO,coordinates(vOCSOL_RO)
  temp2 <- temp
  temp2[temp2>0.2] <- 1
  temp2[temp2<=0.2] <- NA
  temp3 <- temp2 > 1
  
  R_dist <- distanceFromPoints(temp, c(x,y)) 
  crs(R_dist) <- CRS("+init=epsg:2154")
  mapview(temp) + mapview(R_dist)
  length(temp2)
  crs(temp2)
  plot(temp2)

  
  crs(temp) <- CRS("+init=epsg:4326")
  
  temp3 <-  rasterToPolygons(temp2)
  class(temp3)
  temp4 <-  st_as_sf(temp3, coords = c("x", "y"), crs = 4326)
  class(temp4)  
  temp3 <- filter_(temp2, result_test > 0)
  
mapview(temp)+
  mapview(temp2)
  mapview(temp3, aplha = 1)

  file.remove("Projet1/result_test.tif")
  file.remove("Projet1/result_test.tfw")
  
  library(rasterVis); library(raster); library(maptools)
  data(wrld_simpl)
  
  # Create a raster template for rasterizing the polys. 
  r <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, res=1)
  # Rasterize and set land pixels to NA
  r2 <- rasterize(wrld_simpl, r, 1)
  r3 <- mask(is.na(r2), r2, maskvalue=1, updatevalue=NA) 
  length(r3)
  # Calculate distance to nearest non-NA pixel
  d <- distance(r3) # if claculating distances on land instead of ocean: d <- distance(r3)
  # Optionally set non-land pixels to NA (otherwise values are "distance to non-land")
  d <- d*r2 
  levelplot(d/1000, margin=FALSE, at=seq(0, maxValue(d)/1000, length=100),colorkey=list(height=0.6), main='Distance to coast (km)')


r <- raster(ncol=10, nrow=10)
m <- raster(ncol=10, nrow=10)
values(r) <- runif(ncell(r)) * 10
values(m) <- runif(ncell(r))
plot(r)
plot(m)
m[m < 0.9] <- NA
plot(m)
mr <- mask(r, m)
plot(mr)
m2 <- m > .7
mr2 <- mask(r, m2, maskvalue=TRUE)
