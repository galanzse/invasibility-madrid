

# COMPILATION OF PREDICTORS FOR POSTERIOR ANALISES


library(tidyverse)
library(terra)



# rivers, project to get distance in meters
hidrografia <- vect('C:/Users/user/Desktop/DH_Tajo/hi_tramocurso_l_ES030.shp') %>%
  project('epsg:4326') %>% crop(madrid)
# plot(hidrografia)

# vehicle roads
rails <- vect('C:/Users/user/Desktop/Carreteras Madrid/CNIG/RT_FFCC/rt_tramofc_linea.shp')
roads <- vect('C:/Users/user/Desktop/Carreteras Madrid/CNIG/RT_VIARIA/rt_tramo_vial.shp'); crs(roads) <- 'epsg:4258'
roads <- rbind(rails, roads) %>% project('epsg:4326')

# pedestrian and cattle paths
trails <- vect('C:/Users/user/Desktop/Carreteras Madrid/CAM/spacmsendasnaturaleza/SIGI_V_MA_SENDAS_VERDESLine.shp')[,'CDID'] %>% as.polygons()
pecuarias <- vect('C:/Users/user/Desktop/Carreteras Madrid/CAM/spacmvpec2014/SIGI_MA_VIAS_PECUARIASPolygon.shp')[,'CDID']
trails <- rbind(trails, pecuarias) %>% project('epsg:4326'); rm(pecuarias)

# elevation
elevation <- rast('C:/Users/user/Desktop/worldclim/wc2.1_30s_elev/wc2.1_30s_elev.tif')
names(elevation) <- 'elevation'

# mean productivity per raster cell along the study period
Madrid_NPP <- rast('results/Madrid_NPP.tiff')

# worldclim
bioclim <- rast(list.files('C:/Users/user/Desktop/worldclim/wc2.1_30s_bio', full.names=T))
names(bioclim) <- c("bio_1","bio_10","bio_11","bio_12","bio_13","bio_14","bio_15","bio_16","bio_17","bio_18","bio_19","bio_2","bio_3","bio_4","bio_5","bio_6","bio_7","bio_8","bio_9")

# corine
clc_madrid <- c(rast('results/clc_madrid/clc1990.tiff'), rast('results/clc_madrid/clc2000.tiff'), 
                rast('results/clc_madrid/clc2006.tiff'), rast('results/clc_madrid/clc2012.tiff'),
                rast('results/clc_madrid/clc2018.tiff'))
names(clc_madrid) <- c('clc1990', 'clc2000', 'clc2006', 'clc2012', 'clc2018')


