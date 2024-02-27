

# HOMOGENISE CORINE DATASETS FOR POSTERIOR ANALYSES


library(tidyverse)
library(terra)
library(mapSpain)
library(readxl)



# roi
madrid <- esp_get_ccaa('madrid',epsg="3035") %>% vect() %>% buffer(width=5000)

# df to homogenise CORINE versions
rcl_matrix <- read_excel("data/clc_reclassification.xlsx", sheet='rcl_matrix') %>% as.data.frame()
new_levels <- rcl_matrix[,c('becomes','new_label')] %>% unique()


# 1990: import, extract and calculate proportions. Disagg to calculate edges more accurately
clc1990 <- rast('C:/Users/user/Desktop/CORINE accounting layers/eea_r_3035_100_m_clc-1990-acc_p_1986-1998_v18_r05/clc1990_acc_V18_5.tif') %>% terra::crop(madrid) %>% disagg(4)

clc1990 <- classify(clc1990, rcl=as.matrix(rcl_matrix[,c('is','becomes')])) %>%
  as.factor()

levels(clc1990)[[1]] <- merge(levels(clc1990)[[1]], new_levels, by.x='ID', by.y='becomes') %>%
  dplyr::select(ID, new_label)


# 2000: import, extract and calculate proportions
clc2000 <- rast('C:/Users/user/Desktop/CORINE accounting layers/eea_r_3035_100_m_clc-2000-acc_p_1999-2001_v02_r00/CLC2000ACC_V2018_20.tif') %>% terra::crop(madrid) %>% disagg(4)

clc2000 <- classify(clc2000, rcl=as.matrix(rcl_matrix[,c('is','becomes')])) %>%
  as.factor()

levels(clc2000)[[1]] <- merge(levels(clc2000)[[1]], new_levels, by.x='ID', by.y='becomes') %>%
  dplyr::select(ID, new_label)


# 2006: import, extract and calculate proportions
clc2006 <- rast('C:/Users/user/Desktop/CORINE accounting layers/eea_r_3035_100_m_clc-2006-acc_p_2005-2007_v02_r00/CLC2006ACC_V2018_20.tif') %>% terra::crop(madrid) %>% disagg(4)

clc2006 <- classify(clc2006, rcl=as.matrix(rcl_matrix[,c('is','becomes')])) %>%
  as.factor()

levels(clc2006)[[1]] <- merge(levels(clc2006)[[1]], new_levels, by.x='ID', by.y='becomes') %>%
  dplyr::select(ID, new_label)


# 2012: import, extract and calculate proportions
clc2012 <- rast('C:/Users/user/Desktop/CORINE accounting layers/eea_r_3035_100_m_clc-2012-acc_p_2011-2013_v02_r00/CLC2012ACC_V2018_20.tif') %>% terra::crop(madrid) %>% disagg(4)

clc2012 <- classify(clc2012, rcl=as.matrix(rcl_matrix[,c('is','becomes')])) %>%
  as.factor()

levels(clc2012)[[1]] <- merge(levels(clc2012)[[1]], new_levels, by.x='ID', by.y='becomes') %>%
  dplyr::select(ID, new_label)


# 2018: import, extract and calculate proportions
clc2018 <- rast('C:/Users/user/Desktop/CORINE accounting layers/eea_r_3035_100_m_clc-2018-acc_p_2017-2018_v01_r00/CLC2018ACC_V2018_20.tif') %>% terra::crop(madrid) %>% disagg(4)

clc2018 <- classify(clc2018, rcl=as.matrix(rcl_matrix[,c('is','becomes')])) %>%
  as.factor()

levels(clc2018)[[1]] <- merge(levels(clc2018)[[1]], new_levels, by.x='ID', by.y='becomes') %>%
  dplyr::select(ID, new_label)



# stack
clc_madrid <- c(clc1990, clc2000, clc2006, clc2012, clc2018)

# project
names(clc_madrid) <- c('clc1990', 'clc2000', 'clc2006', 'clc2012', 'clc2018')

# save
writeRaster(clc_madrid, 'results/predictors/clc_madrid.tiff', overwrite=T)


