

# COMPARE ATTRIBUTES OF OCCUPIED VS NON-OCCUPIED CELLS AND INTERSPECIFIC DIFFERENCES AMONG NATURALISED SPECIES


library(tidyverse)
# library(terra)


# import data
load("results/atlas_data.RData")
pred_2.5km <- read.csv("results/pred_2.5km.txt", sep="")
table(is.na(pred_2.5km))

# variables of interest
predictors <- colnames(pred_2.5km)[-which(colnames(pred_2.5km)%in%c('MGRS','x','y'))]

# add info invaded/notinvaded
pred_2.5km$invaded <- pred_2.5km$MGRS %in% atlas_data$observations$mgrs
table(pred_2.5km$invaded)

# points df
atlas_pts <- vect(atlas_data$observations, geom=c('x','y')); crs(atlas_pts) <- 'epsg:4326'



# correlations bioclim
# pairs(pred_2.5km[,c("MAT","MTCM","TS","AP","PDQ","PS")],lower.panel=NULL)



# # remove FAR cells for accurate comparison
# roi <- atlas_pts %>% buffer(width=700) %>% aggregate() # plot(atlas_pts); lines(roi)
# roi <- vect('results/mgrs_grid.shp') %>% terra::crop(roi) %>%
#   as.data.frame(xy=T) %>% dplyr::select(MGRS) %>% deframe() %>% unique()
# mgrs_1km <- mgrs_1km %>% subset(MGRS %in% roi) # remove far cells
# table(mgrs_1km$occupied)/nrow(mgrs_1km) # check similar proportion



# df in long format
pred_2.5km_long <- pred_2.5km %>% pivot_longer(all_of(predictors))
pred_2.5km_long$invaded <- as.factor(pred_2.5km_long$invaded)
levels(pred_2.5km_long$invaded) <- c('notinvaded','invaded')
head(pred_2.5km_long)


# plot invaded vs notinvaded
par(mfrow=c(3,3))
for (i in predictors) {
  temp <- pred_2.5km_long %>% dplyr::filter(name==i)
  boxplot(value ~ invaded, data=temp, main=i, xlab='')
}



# plot by life form
pred_2.5km_traits <- atlas_data[['observations']][,c('mgrs','species')] %>%
  merge(atlas_data[['traits']][,c('species_atlas','longevity','lifeform','raunkiaer')], all.x=T,
        by.x='species', by.y='species_atlas') %>%
  merge(pred_2.5km, by.x='mgrs', by.y='MGRS')
pred_2.5km_traits$longevity <- as.factor(pred_2.5km_traits$longevity)
pred_2.5km_traits$lifeform <- as.factor(pred_2.5km_traits$lifeform)
pred_2.5km_traits$raunkiaer <- as.factor(pred_2.5km_traits$raunkiaer)
levels(pred_2.5km_traits$lifeform) <- c('AQ','F','GR','SH','TR')

# plot by longevity
pred_2.5km_traits_long <- pred_2.5km_traits %>% pivot_longer(all_of(predictors))
par(mfrow=c(3,3))
for (i in predictors) {
  temp <- pred_2.5km_traits_long %>% dplyr::filter(name==i)
  boxplot(value ~ longevity, data=temp, main=i, xlab='')
}

# plot by lifeform
par(mfrow=c(3,3))
for (i in predictors) {
  temp <- pred_2.5km_traits_long %>% dplyr::filter(name==i)
  boxplot(value ~ lifeform, data=temp, main=i, xlab='')
}


