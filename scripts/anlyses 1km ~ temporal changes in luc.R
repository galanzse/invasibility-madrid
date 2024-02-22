

# CALCULATE TEMPORAL CHANGES IN CLC_LUC AND TRANSITION MATRICES FOR EVERY 1KM MGRS CELL IN MADRID


library(tidyverse)
library(readxl)
library(terra)
library(mapSpain)
library(landscapemetrics)


# references
# Feranec et al. 2010 (10.1016/j.apgeog.2009.07.003)
# https://bbest.github.io/landscape-ecology-labs/lab2.html



# corine
clc_madrid <- c(rast('results/clc_madrid/clc1990.tiff'), rast('results/clc_madrid/clc2000.tiff'), 
                rast('results/clc_madrid/clc2006.tiff'), rast('results/clc_madrid/clc2012.tiff'),
                rast('results/clc_madrid/clc2018.tiff'))
names(clc_madrid) <- c('clc1990', 'clc2000', 'clc2006', 'clc2012', 'clc2018')


# calculate predictors for every MGRS cell
load("C:/Users/user/OneDrive/ACADEMICO/proyectos/ailanthus/results/atlas_data.RData")
mgrs_df <- atlas_data[['mgrs_centroids']] # dataframe to store results
mgrs_grid <- vect('results/mgrs_grid.shp') # shapefile to extract polygons from


# levels and years df
new_levels <- expand.grid(c(1990,2000,2006,2012,2018), 1:12) %>%
  merge(levels(clc_madrid$clc1990)[[1]], by.x='Var2', by.y='value') %>%
  merge(data.frame(layer=1:5, year=c(1990,2000,2006,2012,2018)), by.x='Var1', by.y='year')
colnames(new_levels) <- c('year','class','label','layer')

# transition references
clc_reclassification <- read_excel("data/clc_reclassification.xlsx", sheet="transition", n_max=12) %>%
  as.data.frame()
rownames(clc_reclassification) <- clc_reclassification$class; clc_reclassification$class <- NULL 
clc_reclassification <- as.matrix(clc_reclassification)


# extract info from within polygons and save into mgrs_df ###
for (i in 1:nrow(mgrs_df)) {
  
  # mgrs polygon
  pol1 <- mgrs_grid %>% terra::subset(mgrs_grid$MGRS==mgrs_df$MGRS[i]) %>%
    project('epsg:3035')
  
  # crop
  clc_roi <- clc_madrid %>% crop(pol1, mask=T)

  
  
  # evolution in landscape metrics
  # median landscape metrics
  temp_landscape <- calculate_lsm(clc_roi, metric=c('pr','pd','shdi','ed'), level='landscape') %>%
    dplyr::select(layer, metric, value) %>%
    merge(new_levels[,c('year','layer')]) %>% dplyr::select(-layer) %>%
    filter(year!=2006)
  
  # include period to group_by
  temp_landscape$period[temp_landscape$year%in%c(1990,2000)] <- 'before'
  temp_landscape$period[temp_landscape$year%in%c(2012,2018)] <- 'after'
  
  # summarise
  temp_landscape <- temp_landscape %>% group_by(metric, period) %>%
    summarise(value=mean(value), .groups='drop_last') %>%
    pivot_wider(names_from=period, values_from=value)
  
  # calculate difference and save
  temp_landscape$diff <-  temp_landscape$after - temp_landscape$before
  
  mgrs_df$d_l_pr[i] <- temp_landscape$diff[temp_landscape$metric=='pr'] # n of classes
  mgrs_df$d_l_pd[i] <- temp_landscape$diff[temp_landscape$metric=='pd'] # n of patches/area
  mgrs_df$d_l_shdi[i] <- temp_landscape$diff[temp_landscape$metric=='shdi'] # shannon
  mgrs_df$d_l_ed[i] <- temp_landscape$diff[temp_landscape$metric=='ed'] # edge / area

  
  
  # difference in area within land uses: mean(2012-2018) - mean(1990-2000)
    # extract areas and merge with reference df
    df_c_ca <- lsm_c_ca(clc_roi) %>%
      dplyr::select(layer, class, value) %>%
      merge(new_levels, all.y=T) %>%
      dplyr::select(value, year, label) %>%
      filter(year!=2006)

    # replace NA
    df_c_ca$value[is.na(df_c_ca$value)] <- 0

    # standardise by area
    df_c_ca$value <- df_c_ca$value/deframe(lsm_l_ta(clc_roi)[1,'value'])

    # include period to group_by
    df_c_ca$period[df_c_ca$year%in%c(1990,2000)] <- 'before'
    df_c_ca$period[df_c_ca$year%in%c(2012,2018)] <- 'after'
    
    # summarise
    df_c_ca <- df_c_ca %>% group_by(label, period) %>%
      summarise(value=mean(value), .groups='drop_last') %>% na.omit() %>%
      pivot_wider(names_from=period, values_from=value)
    
    # calculate difference and save
    df_c_ca$diff <-  df_c_ca$after - df_c_ca$before
    
    mgrs_df$d_arable[i] <- df_c_ca$diff[df_c_ca$label=='Arable land']
    mgrs_df$d_complexc[i] <- df_c_ca$diff[df_c_ca$label=='Complex cultivation']
    mgrs_df$d_greenurban[i] <- df_c_ca$diff[df_c_ca$label=='Green urban areas']
    mgrs_df$d_industrial[i] <- df_c_ca$diff[df_c_ca$label=='Industrial and transport']
    mgrs_df$d_grasslands[i] <- df_c_ca$diff[df_c_ca$label=='Natural grasslands']
    mgrs_df$d_woodlands[i] <- df_c_ca$diff[df_c_ca$label=='Natural woodlands']
    mgrs_df$d_other[i] <- df_c_ca$diff[df_c_ca$label=='Other']
    mgrs_df$d_pastures[i] <- df_c_ca$diff[df_c_ca$label=='Pastures']
    mgrs_df$d_crops[i] <- df_c_ca$diff[df_c_ca$label=='Permanent crops']
    mgrs_df$d_seminatural[i] <- df_c_ca$diff[df_c_ca$label=='Seminatural']
    mgrs_df$d_urban[i] <- df_c_ca$diff[df_c_ca$label=='Urban']

  
    
  # transition matrix
    # get raw tally matrix from 2001 to 2006
      v = data.frame(
        clc1990 = values(clc_roi$clc1990),
        clc2000 = values(clc_roi$clc2000),
        clc2006 = values(clc_roi$clc2006),
        clc2012 = values(clc_roi$clc2012),
        clc2018 = values(clc_roi$clc2018))
  
    # reclassify
      for (o in 1:nrow(new_levels)) { v[v==new_levels$class[o]] <- new_levels$label[o] }
  
    # raw tally matrix
      m = table(v[,c('clc1990','clc2018')]) # 6320

    # calculate transition matrix
      P = as.matrix(m / sum(m))
      
    # crop reference matrix
      P_ref <- clc_reclassification[rownames(P),colnames(P)]
  
    # retrieve transition data
      mgrs_df$LCF1[i] <- P[P_ref==1] %>% sum()
      mgrs_df$LCF2[i] <- P[P_ref==2] %>% sum()
      mgrs_df$LCF3[i] <- P[P_ref==3] %>% sum()
      mgrs_df$LCF4[i] <- P[P_ref==4] %>% sum()
      mgrs_df$LCF5[i] <- P[P_ref==5] %>% sum()
      mgrs_df$LCF6[i] <- P[P_ref==6] %>% sum()

      
      
  # progress
  print(paste(round(i/nrow(mgrs_df)*100,2), '%', sep=''))
  
}

# save data.frame
write.table(mgrs_df, 'results/mgrs_transition_1km.txt')


