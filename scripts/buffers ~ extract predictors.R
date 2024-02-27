

# EXTRACT PREDICTORS FOR EVERY MGRS CELL IN MADRID USING DIFFERENT BUFFERS


library(tidyverse)
library(readxl)
library(terra)
library(mapSpain)
library(landscapemetrics)



# study area
madrid <- esp_get_ccaa('madrid',epsg="4326") %>% vect(); crs(madrid) <- 'epsg:4326'

# predictors
rivers <- vect('results/predictors/rivers.shp')
roads <- vect('results/predictors/roads.shp')
trails <- vect('results/predictors/trails.shp')
elevation <- rast('results/predictors/elevation.tiff')
NPP <- rast('results/predictors/MAdrid_NPP.tiff')
bioclim <- rast('results/predictors/bioclim.tiff')
corine <- rast('results/predictors/clc_madrid.tiff')



# CLC levels for data wrangling
new_levels <- expand.grid(c(1990,2000,2006,2012,2018), 1:12) %>%
  merge(levels(corine$clc1990)[[1]], by.x='Var2', by.y='value') %>%
  merge(data.frame(layer=1:5, year=c(1990,2000,2006,2012,2018)), by.x='Var1', by.y='year')
colnames(new_levels) <- c('year','class','label','layer')

# calculate predictors for every MGRS cell
load("results/atlas_data.RData")

# transition matrix reference
clc_reclassification <- read_excel("data/clc_reclassification.xlsx", sheet="transition", n_max=12) %>% as.data.frame()
rownames(clc_reclassification) <- clc_reclassification$class; clc_reclassification$class <- NULL
clc_reclassification <- as.matrix(clc_reclassification)



# extract info from within polygons and save into mgrs_df ###
for (r in c(0.5,1,2.5,5)) { # buffer size in km
  
  # dataframe to store results
  mgrs_df <- atlas_data[['mgrs_centroids']] 
  
  for (i in 1:nrow(mgrs_df)) {
    

    # for physical, transport and climate variables lets use the mgrs grid to account for a local effect
    pol1 <- mgrs_grid %>% terra::subset(mgrs_grid$MGRS==mgrs_df$MGRS[i])
    
    
    # river length in meters within roi
    mgrs_df$stream_dens[i] <- terra::crop(rivers, pol1) %>% perim() %>% sum()
    
    # road and trail length in meters within roi
    mgrs_df$road_dens[i] <- crop(roads,pol1) %>% perim() %>% sum()
    mgrs_df$trail_dens[i] <- crop(trails,pol1) %>% perim() %>% sum()
    
    # average elevation
    mgrs_df$elevation[i] <- terra::extract(x=elevation, y=pol1) %>% dplyr::select(elevation) %>% colMeans(na.rm=T)
    
    # net primary productivity
    mgrs_df$NPP[i] <- NPP %>% terra::extract(y=pol1) %>% dplyr::select(annual_productivity) %>% colMeans(na.rm=T)
    
    # climate characterization
    mgrs_df$MAT[i] <- terra::extract(x=bioclim$bio_1, y=pol1) %>% dplyr::select(bio_1) %>% colMeans(na.rm=T)
    mgrs_df$MTCM[i] <- terra::extract(x=bioclim$bio_6, y=pol1) %>% dplyr::select(bio_6) %>% colMeans(na.rm=T)
    mgrs_df$TS[i] <- terra::extract(x=bioclim$bio_4, y=pol1) %>% dplyr::select(bio_4) %>% colMeans(na.rm=T)
    mgrs_df$AP[i] <- terra::extract(x=bioclim$bio_12, y=pol1) %>% dplyr::select(bio_12) %>% colMeans(na.rm=T)
    mgrs_df$PDQ[i] <- terra::extract(x=bioclim$bio_17, y=pol1) %>% dplyr::select(bio_17) %>% colMeans(na.rm=T)
    mgrs_df$PS[i] <- terra::extract(x=bioclim$bio_15, y=pol1) %>% dplyr::select(bio_15) %>% colMeans(na.rm=T)
    
    

    # for land use lets consider different buffers
    if (r<1) { pol1 <- mgrs_grid %>% terra::subset(mgrs_grid$MGRS==mgrs_df$MGRS[i])
    
    } else { pol1 <- vect(mgrs_df[i,], geom=c('x','y'), 'epsg:4326') %>% buffer(width=r*1000) }
    
    
    # landscape metrics: crop CLC
    clc_roi <- corine %>% crop(terra::project(pol1, 'epsg:3035'), mask=T) # check_landscape(clc_roi)
    
    # accumulated land uses since 1990
    clc_roi_df <- clc_roi %>% as.data.frame() %>% as.list()
    mgrs_df$n_luc[i] <- do.call(c, clc_roi_df) %>% unique() %>% length()
    
    # compute landscape metrics and standardised area
    landscape_all <- calculate_lsm(clc_roi, metric=c('pr','pd','shdi','ed','ca'), level=c('landscape','class'))
    landscape_area <- lsm_l_ta(clc_roi) %>% summarise(value=mean(value)) %>% deframe()
    landscape_all$value[landscape_all$metric=='ca'] <- landscape_all$value[landscape_all$metric=='ca']/landscape_area
    
    
    # mean landscape metrics over time series
    landscape_means <- landscape_all %>% filter(level=='landscape') %>% group_by(metric) %>% summarise(value=mean(value))
    mgrs_df$l_pr[i] <- landscape_means$value[landscape_means$metric=='pr'] # n of classes
    mgrs_df$l_pd[i] <- landscape_means$value[landscape_means$metric=='pd'] # n of patches/area
    mgrs_df$l_shdi[i] <- landscape_means$value[landscape_means$metric=='shdi'] # shannon
    mgrs_df$l_ed[i] <- landscape_means$value[landscape_means$metric=='ed'] # edge / area
    
    
    # mean class areas over time series including zeroes
    df_c_ca <- landscape_all %>% filter(level=='class' & metric=='ca') %>% dplyr::select(-id, -level) %>%
      merge(unique(new_levels[,c('year','layer','class','label')]), all.y=T) %>%
      dplyr::select(-layer, -class, -metric)
    
    # remove zeroes and compute means
    df_c_ca$value[is.na(df_c_ca$value)] <- 0
    df_c_ca <- aggregate(value~label, FUN=mean, df_c_ca) 
    
    # extract areas
    mgrs_df$ca_arable[i] <- df_c_ca$value[df_c_ca$label=='Arable land']
    mgrs_df$ca_greenurban[i] <- df_c_ca$value[df_c_ca$label=='Green urban areas']
    mgrs_df$ca_heterogagric[i] <- df_c_ca$value[df_c_ca$label=='Heterogeneous agricultural']
    mgrs_df$ca_industrial[i] <- df_c_ca$value[df_c_ca$label=='Industrial and transport']
    mgrs_df$ca_woodlands[i] <- df_c_ca$value[df_c_ca$label=='Natural woodlands']
    mgrs_df$ca_other[i] <- df_c_ca$value[df_c_ca$label=='Other']
    mgrs_df$ca_pastures[i] <- df_c_ca$value[df_c_ca$label=='Pastures']
    mgrs_df$ca_crops[i] <- df_c_ca$value[df_c_ca$label=='Permanent crops']
    mgrs_df$ca_scrub[i] <- df_c_ca$value[df_c_ca$label=='Scrub and herbaceous']
    mgrs_df$ca_urban[i] <- df_c_ca$value[df_c_ca$label=='Urban']
    
    
    
    # evolution of landscape metrics
    landscape_evolution <- landscape_all %>% filter(level=='landscape') %>%
      dplyr::select(layer, metric, value) %>%
      merge(unique(new_levels[,c('year','layer')])) %>% dplyr::select(-layer)
    
    # include period to group_by, summarise and calculate difference
    landscape_evolution$period[landscape_evolution$year%in%c(1990,2000)] <- 'old'
    landscape_evolution$period[landscape_evolution$year%in%c(2012,2018)] <- 'recent'
    landscape_evolution <- landscape_evolution %>% group_by(metric, period) %>%
      summarise(value=mean(value), .groups='drop_last') %>% na.omit %>%
      pivot_wider(names_from=period, values_from=value)
    landscape_evolution$diff <-  landscape_evolution$recent - landscape_evolution$old
    
    # extract difference
    mgrs_df$d_l_pr[i] <- landscape_evolution$diff[landscape_evolution$metric=='pr']
    mgrs_df$d_l_pd[i] <- landscape_evolution$diff[landscape_evolution$metric=='pd']
    mgrs_df$d_l_shdi[i] <- landscape_evolution$diff[landscape_evolution$metric=='shdi']
    mgrs_df$d_l_ed[i] <- landscape_evolution$diff[landscape_evolution$metric=='ed']
    
    
    # evolution of class areas
    df_c_ca <- landscape_all %>% filter(level=='class' & metric=='ca') %>%
      dplyr::select(layer, class, value) %>%
      merge(new_levels, all.y=T) %>%
      dplyr::select(value, year, label)
    
    # replace NA, include period to group_by, summarise and calculate difference
    df_c_ca$value[is.na(df_c_ca$value)] <- 0
    df_c_ca$period[df_c_ca$year%in%c(1990,2000)] <- 'old'
    df_c_ca$period[df_c_ca$year%in%c(2012,2018)] <- 'recent'
    df_c_ca <- df_c_ca %>% na.omit() %>% group_by(label, period) %>%
      summarise(value=mean(value), .groups='drop_last') %>%
      pivot_wider(names_from=period, values_from=value)
    df_c_ca$diff <-  df_c_ca$recent - df_c_ca$old
    
    #  extract difference
    mgrs_df$dif_arable[i] <- df_c_ca$diff[df_c_ca$label=='Arable land']
    mgrs_df$dif_greenurban[i] <- df_c_ca$diff[df_c_ca$label=='Green urban areas']
    mgrs_df$dif_heterogagric[i] <- df_c_ca$diff[df_c_ca$label=='Heterogeneous agricultural']
    mgrs_df$dif_industrial[i] <- df_c_ca$diff[df_c_ca$label=='Industrial and transport']
    mgrs_df$dif_woodlands[i] <- df_c_ca$diff[df_c_ca$label=='Natural woodlands']
    mgrs_df$dif_other[i] <- df_c_ca$diff[df_c_ca$label=='Other']
    mgrs_df$dif_pastures[i] <- df_c_ca$diff[df_c_ca$label=='Pastures']
    mgrs_df$dif_crops[i] <- df_c_ca$diff[df_c_ca$label=='Permanent crops']
    mgrs_df$dif_scrub[i] <- df_c_ca$diff[df_c_ca$label=='Scrub and herbaceous']
    mgrs_df$dif_urban[i] <- df_c_ca$diff[df_c_ca$label=='Urban']
    
    
    # changes among land uses: get raw tally matrix for every period
    v = data.frame(clc2000 = values(clc_roi$clc2000), clc2018 = values(clc_roi$clc2018)) %>% na.omit()
    # reclassify
    for (o in 1:nrow(new_levels)) { v[v==new_levels$class[o]] <- new_levels$label[o] }
    # raw tally matrix
    m = table(v[,c('clc2000','clc2018')])
    # calculate transition matrix
    P = as.matrix(m/sum(m))
    # crop reference matrix
    P_ref <- clc_reclassification[rownames(P),colnames(P)]
    
    # retrieve transition data
    mgrs_df$LCF1[i] <- P[P_ref==1] %>% sum()
    mgrs_df$LCF2[i] <- P[P_ref==2] %>% sum()
    mgrs_df$LCF3[i] <- P[P_ref==3] %>% sum()
    mgrs_df$LCF4[i] <- P[P_ref==4] %>% sum()
    mgrs_df$LCF5[i] <- P[P_ref==5] %>% sum()
    
    
    # progress
    print(paste(round(i/nrow(mgrs_df)*100,2), '% --- buffer=', r, 'km', sep=''))
    
  }
  
  # save data.frame
  write.table(mgrs_df, paste('results/pred_', r, 'km.txt', sep=''))
  
}


