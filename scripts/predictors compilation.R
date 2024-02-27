

# COMPILATION OF PREDICTORS FOR POSTERIOR ANALISES


library(tidyverse)
library(terra)
library(MODISTools)



# study area
madrid <- esp_get_ccaa('madrid',epsg="4326") %>% vect(); crs(madrid) <- 'epsg:4326'



# rivers, project to get distance in meters
rivers <- vect('C:/Users/user/Desktop/DH_Tajo/hi_tramocurso_l_ES030.shp') %>%
  project('epsg:4326') %>% crop(madrid)
# plot(rivers)

# vehicle roads
rails <- vect('C:/Users/user/Desktop/Carreteras Madrid/CNIG/RT_FFCC/rt_tramofc_linea.shp')
roads <- vect('C:/Users/user/Desktop/Carreteras Madrid/CNIG/RT_VIARIA/rt_tramo_vial.shp'); crs(roads) <- 'epsg:4258'
roads <- rbind(rails, roads) %>% project('epsg:4326')

# pedestrian and cattle paths
trails <- vect('C:/Users/user/Desktop/Carreteras Madrid/CAM/spacmsendasnaturaleza/SIGI_V_MA_SENDAS_VERDESLine.shp')[,'CDID'] %>% as.polygons()
pecuarias <- vect('C:/Users/user/Desktop/Carreteras Madrid/CAM/spacmvpec2014/SIGI_MA_VIAS_PECUARIASPolygon.shp')[,'CDID']
trails <- rbind(trails, pecuarias) %>% project('epsg:4326'); rm(pecuarias)

# elevation
elevation <- rast('C:/Users/user/Desktop/worldclim/wc2.1_30s_elev/wc2.1_30s_elev.tif') %>% crop(madrid)
names(elevation) <- 'elevation'

# worldclim
bioclim <- rast(list.files('C:/Users/user/Desktop/worldclim/wc2.1_30s_bio', full.names=T)) %>% crop(madrid)
names(bioclim) <- c("bio_1","bio_10","bio_11","bio_12","bio_13","bio_14","bio_15","bio_16","bio_17","bio_18","bio_19","bio_2","bio_3","bio_4","bio_5","bio_6","bio_7","bio_8","bio_9")



# EVI2 (productivity): 2002, 2006, 2012, 2018 [MODIS 500m: MCD12Q2]
View(mt_products())
View(mt_bands(product = "MYD17A3HGF"))
mt_dates(product = "MCD12Q1", lat = 40.395898, lon = -3.703260)
modis_sinusoidal <- '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs'

madrid_NPP <- mt_subset(product="MYD17A3HGF", band='Npp_500m',
                        start="2002-01-01", end="2020-12-31",
                        lat=40.495, lon=-3.71686,
                        km_lr=100, km_ab=100,
                        site_name="Madrid",
                        internal=TRUE, progress=TRUE)

madrid_NPP$value[madrid_NPP$value==32767] <- NA # remove outliers

# convert into raster and calculate mean
Madrid_NPP <- madrid_NPP %>% mt_to_terra(reproject=F) %>%  mean(na.rm=T)
names(Madrid_NPP) <- 'annual_productivity'

# remove outliers
Madrid_NPP[Madrid_NPP>1] <- 0
Madrid_NPP[is.na(Madrid_NPP)] <- 0

# reproject and plot
Madrid_NPP <- Madrid_NPP %>% project('epsg:4326')
plot(Madrid_NPP); lines(madrid)



# write predictors into folder
writeVector(rivers, 'results/predictors/rivers.shp', overwrite=T)
writeVector(roads, 'results/predictors/roads.shp', overwrite=T)
writeVector(trails, 'results/predictors/trails.shp', overwrite=T)
writeRaster(elevation, 'results/predictors/elevation.tiff', overwrite=T)
writeRaster(bioclim, 'results/predictors/bioclim.tiff', overwrite=T)
writeRaster(Madrid_NPP, 'results/predictors/Madrid_NPP.tiff', overwrite=T)


