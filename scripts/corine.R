

# HOMOGENISE CORINE DATASETS FOR POSTERIOR ANALYSES


library(tidyverse)
library(terra)
library(mapSpain)
library(readxl)



# roi
madrid <- esp_get_ccaa('madrid',epsg="3035") %>% vect() %>% buffer(width=5000)

# df to homogenise CORINE versions
rcl_matrix <- read_excel("data/clc_reclassification.xlsx") %>% as.data.frame()
new_levels <- rcl_matrix[,c('becomes','new_label')] %>% unique()


# 1990: import, extract and calculate proportions. Disagg to calculate edges more accurately
clc1990 <- rast('E:/CORINE/u2000_clc1990_v2020_20u1_raster100m/u2000_clc1990_v2020_20u1_raster100m/DATA/U2000_CLC1990_V2020_20u1.tif') %>%
  terra::crop(madrid) %>% disagg(4)

clc1990 <- classify(clc1990, rcl=as.matrix(rcl_matrix[,c('is','becomes')])) %>%
  as.factor()

levels(clc1990)[[1]] <- merge(levels(clc1990)[[1]], new_levels, by.x='ID', by.y='becomes') %>%
  dplyr::select(ID, new_label)


# 2000: import, extract and calculate proportions
clc2000 <- rast('E:/CORINE/u2006_clc2000_v2020_20u1_raster100m/DATA/U2006_CLC2000_V2020_20u1.tif') %>%
  terra::crop(madrid) %>% disagg(4)

clc2000 <- classify(clc2000, rcl=as.matrix(rcl_matrix[,c('is','becomes')])) %>%
  as.factor()

levels(clc2000)[[1]] <- merge(levels(clc2000)[[1]], new_levels, by.x='ID', by.y='becomes') %>%
  dplyr::select(ID, new_label)


# 2006: import, extract and calculate proportions
clc2006 <- rast('E:/CORINE/u2012_clc2006_v2020_20u1_raster100m/DATA/U2012_CLC2006_V2020_20u1.tif') %>%
  terra::crop(madrid) %>% disagg(4)

clc2006 <- classify(clc2006, rcl=as.matrix(rcl_matrix[,c('is','becomes')])) %>%
  as.factor()

levels(clc2006)[[1]] <- merge(levels(clc2006)[[1]], new_levels, by.x='ID', by.y='becomes') %>%
  dplyr::select(ID, new_label)


# 2012: import, extract and calculate proportions
clc2012 <- rast('E:/CORINE/u2018_clc2012_v2020_20u1_raster100m/DATA/U2018_CLC2012_V2020_20u1.tif') %>%
  terra::crop(madrid) %>% disagg(4)

clc2012 <- classify(clc2012, rcl=as.matrix(rcl_matrix[,c('is','becomes')])) %>%
  as.factor()

levels(clc2012)[[1]] <- merge(levels(clc2012)[[1]], new_levels, by.x='ID', by.y='becomes') %>%
  dplyr::select(ID, new_label)


# 2018: import, extract and calculate proportions
clc2018 <- rast('E:/CORINE/u2018_clc2018_v2020_20u1_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif') %>%
  terra::crop(madrid) %>% disagg(4)

clc2018 <- classify(clc2018, rcl=as.matrix(rcl_matrix[,c('is','becomes')])) %>%
  as.factor()

levels(clc2018)[[1]] <- merge(levels(clc2018)[[1]], new_levels, by.x='ID', by.y='becomes') %>%
  dplyr::select(ID, new_label)



# stack
clc_madrid <- c(clc1990, clc2000, clc2006, clc2012, clc2018)

# project
names(clc_madrid) <- c('clc1990', 'clc2000', 'clc2006', 'clc2012', 'clc2018')

# save
writeRaster(clc_madrid$clc1990, file='results/clc_madrid/clc1990.tiff', overwrite=TRUE)
writeRaster(clc_madrid$clc2000, file='results/clc_madrid/clc2000.tiff', overwrite=TRUE)
writeRaster(clc_madrid$clc2006, file='results/clc_madrid/clc2006.tiff', overwrite=TRUE)
writeRaster(clc_madrid$clc2012, file='results/clc_madrid/clc2012.tiff', overwrite=TRUE)
writeRaster(clc_madrid$clc2018, file='results/clc_madrid/clc2018.tiff', overwrite=TRUE)


