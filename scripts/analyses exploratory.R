

# COMPARE ATTRIBUTES OF OCCUPIED VS NON-OCCUPIED CELLS AND INTERSPECIFIC DIFFERENCES AMONG NATURALISED SPECIES


library(tidyverse)
library(terra)



# import data
load("C:/Users/user/OneDrive/ACADEMICO/proyectos/ailanthus/results/atlas_data.RData")
mgrs_1km <- read.csv("results/mgrs_pred_1km.txt", sep="")
atlas_pts <- vect(atlas_data[['observations']], geom=c('x','y')); crs(atlas_pts) <- 'epsg:4326'


# exploratory
table(is.na(mgrs_1km))

# correlations bioclim
# pairs(mgrs_1km[,c("MAT","MTCM","TS","AP","PDQ","PS")]) 

# variables of interes
predictors <- colnames(mgrs_1km)[-which(colnames(mgrs_1km)%in%c('MGRS','x','y','occupied'))]



# first, explore proportion of occupied cells considering ALL cells
occ_cells <- atlas_data[['observations']]$mgrs %>% unique() # occupied cells
mgrs_1km$occupied[mgrs_1km$MGRS %in% occ_cells] <- 'y' # create new variable
mgrs_1km$occupied[!(mgrs_1km$MGRS %in% occ_cells)] <- 'n'
table(mgrs_1km$occupied)/nrow(mgrs_1km) # proportion of occupied cells

# second, remove FAR cells for accurate comparison
roi <- atlas_pts %>% buffer(width=700) %>% aggregate() # plot(atlas_pts); lines(roi)
roi <- vect('results/mgrs_grid.shp') %>% terra::crop(roi) %>%
  as.data.frame(xy=T) %>% dplyr::select(MGRS) %>% deframe() %>% unique()
mgrs_1km <- mgrs_1km %>% subset(MGRS %in% roi) # remove far cells
table(mgrs_1km$occupied)/nrow(mgrs_1km) # check similar proportion

par(mfrow=c(3,3)) # plot
for (i in predictors) {
  temp <- mgrs_1km %>% dplyr::select(occupied, i)
  colnames(temp) <- c('occupied','predictor')
  boxplot(predictor ~ occupied, data=temp, main=i) 
}



# diferencias entre formas de vida
diff_lifeforms <- atlas_data[['observations']][,c('mgrs','species')]
diff_lifeforms$longevity <- as.factor(diff_lifeforms$longevity)
diff_lifeforms$life_form <- as.factor(diff_lifeforms$life_form)
levels(diff_lifeforms$life_form) <- c('C','G','HC','HY','PC','PS','PT','TH')

diff_lifeforms <- diff_lifeforms %>%
  merge(atlas_data[['traits']][,c('species_atlas','longevity','life_form')], all.x=T,
        by.x='species', by.y='species_atlas') %>%
  merge(mgrs_1km, by.x='mgrs', by.y='MGRS')

par(mfrow=c(3,3)) # plot
for (i in predictors) {
  temp <- diff_lifeforms %>% dplyr::select(longevity, i)
  colnames(temp) <- c('longevity','predictor')
  boxplot(predictor ~ longevity, data=temp, main=i) 
}

par(mfrow=c(3,3)) # plot
for (i in predictors) {
  temp <- diff_lifeforms %>% dplyr::select(life_form, i)
  levels(temp$life_form)
  colnames(temp) <- c('life_form','predictor')
  boxplot(predictor ~ life_form, data=temp, main=i) 
}





