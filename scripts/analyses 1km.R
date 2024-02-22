

# EXTRACT AVERAGE PREDICTORS FOR EVERY 1KM MGRS CELL IN MADRID


library(tidyverse)
library(readxl)
library(terra)
library(mapSpain)
library(landscapemetrics)



# study area
madrid <- esp_get_ccaa('madrid',epsg="4326") %>% vect(); crs(madrid) <- 'epsg:4326'

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
new_levels <- levels(clc_madrid$clc1990)[[1]]; colnames(new_levels) <- c('value','label')



# calculate predictors for every MGRS cell
load("C:/Users/user/OneDrive/ACADEMICO/proyectos/ailanthus/results/atlas_data.RData")
mgrs_df <- atlas_data[['mgrs_centroids']] # dataframe to store results
mgrs_grid <- vect('results/mgrs_grid.shp') # shapefile to extract polygons from

# extract info from within polygons and save into mgrs_df ###
for (i in 1:nrow(mgrs_df)) {
  
  # mgrs polygon
  pol1 <- mgrs_grid %>% terra::subset(mgrs_grid$MGRS==mgrs_df$MGRS[i])
  
  # rivers
  mgrs_df$strem_dens[i] <- terra::crop(hidrografia, pol1) %>% perim() %>% sum()

  # roads
  mgrs_df$road_dens[i] <- crop(roads,pol1) %>% perim() %>% sum()
  mgrs_df$trail_dens[i] <- crop(trails,pol1) %>% perim() %>% sum()

  # elevation
  mgrs_df$elevation[i] <- terra::extract(x=elevation, y=pol1) %>% dplyr::select(elevation) %>% colMeans(na.rm=T)

  # NPP
  mgrs_df$NPP[i] <- Madrid_NPP %>% terra::extract(y=pol1) %>% dplyr::select(annual_productivity) %>% colMeans(na.rm=T)

  # bioclim
  mgrs_df$MAT[i] <- terra::extract(x=bioclim$bio_1, y=pol1) %>% dplyr::select(bio_1) %>% colMeans(na.rm=T)
  mgrs_df$MTCM[i] <- terra::extract(x=bioclim$bio_6, y=pol1) %>% dplyr::select(bio_6) %>% colMeans(na.rm=T)
  mgrs_df$TS[i] <- terra::extract(x=bioclim$bio_4, y=pol1) %>% dplyr::select(bio_4) %>% colMeans(na.rm=T)
  mgrs_df$AP[i] <- terra::extract(x=bioclim$bio_12, y=pol1) %>% dplyr::select(bio_12) %>% colMeans(na.rm=T)
  mgrs_df$PDQ[i] <- terra::extract(x=bioclim$bio_17, y=pol1) %>% dplyr::select(bio_17) %>% colMeans(na.rm=T)
  mgrs_df$PS[i] <- terra::extract(x=bioclim$bio_15, y=pol1) %>% dplyr::select(bio_15) %>% colMeans(na.rm=T)
  

  # landscape metrics
  pol1 <- pol1 %>% project('epsg:3035') # work projected
  clc_roi <- clc_madrid %>% crop(pol1, mask=T)
  # check_landscape(clc_roi)
  
  # historic accumulated land uses
  clc_roi_df <- clc_roi %>% as.data.frame()
  mgrs_df$n_luc[i] <- c(clc_roi_df$clc1990, clc_roi_df$clc2000, clc_roi_df$clc2006, clc_roi_df$clc2012, clc_roi_df$clc2018) %>% unique() %>% length()

  # median landscape metrics
  temp_landscape <- calculate_lsm(clc_roi, metric=c('pr','pd','shdi','ed'), level='landscape') %>%
    dplyr::select(metric, value) %>% na.omit() # compute indices in paralel
  temp_landscape <- aggregate(value~metric, FUN=median, data=temp_landscape) # median per index
  mgrs_df$l_pr[i] <- temp_landscape$value[temp_landscape$metric=='pr'] # n of classes
  mgrs_df$l_pd[i] <- temp_landscape$value[temp_landscape$metric=='pd'] # n of patches/area
  mgrs_df$l_shdi[i] <- temp_landscape$value[temp_landscape$metric=='shdi'] # shannon
  mgrs_df$l_ed[i] <- temp_landscape$value[temp_landscape$metric=='ed'] # edge / area
  
  
  df_c_ca <- lsm_c_ca(clc_roi) %>% dplyr::select(metric, value, class) %>% na.omit() %>%
    merge(new_levels, by.x='class', by.y='value', all.y=T) %>% dplyr::select(label, value)
  df_c_ca$value[is.na(df_c_ca$value)] <- 0
  df_c_ca <- aggregate(value~label, FUN=median, data=df_c_ca)
  
  # median proportional land use
  mgrs_df$ca_arable[i] <- df_c_ca$value[df_c_ca$label=='Arable land']
  mgrs_df$ca_complexc[i] <- df_c_ca$value[df_c_ca$label=='Complex cultivation']
  mgrs_df$ca_greenurban[i] <- df_c_ca$value[df_c_ca$label=='Green urban areas']
  mgrs_df$ca_industrial[i] <- df_c_ca$value[df_c_ca$label=='Industrial and transport']
  mgrs_df$ca_grasslands[i] <- df_c_ca$value[df_c_ca$label=='Natural grasslands']
  mgrs_df$ca_woodlands[i] <- df_c_ca$value[df_c_ca$label=='Natural woodlands']
  mgrs_df$ca_other[i] <- df_c_ca$value[df_c_ca$label=='Other']
  mgrs_df$ca_pastures[i] <- df_c_ca$value[df_c_ca$label=='Pastures']
  mgrs_df$ca_crops[i] <- df_c_ca$value[df_c_ca$label=='Permanent crops']
  mgrs_df$ca_seminatural[i] <- df_c_ca$value[df_c_ca$label=='Seminatural']
  mgrs_df$ca_urban[i] <- df_c_ca$value[df_c_ca$label=='Urban']


  # progress
  print(paste(round(i/nrow(mgrs_df)*100,2), '%', sep=''))
  
}

# save data.frame
write.table(mgrs_df, 'results/mgrs_pred_1km.txt')


