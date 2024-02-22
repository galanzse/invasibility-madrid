

# IMPORT OBSERVATIONS FROM ATLAS (Doi.org/10.5209/bocm.67409), GET MGRS CENTROIDS AND ADD TAXONOMY AND LIFE FORM


library(tidyverse)
library(readxl)
library(rgbif)
# library(TR8)
library(terra)
library(mapSpain)



# observations
atlas_obs <- read_excel("data/AtlasFloraAloctonaMadrid.xlsx", sheet='observations')

# study area
madrid <- esp_get_ccaa('madrid',epsg="4326") %>% vect()

# keep Madrid grid (entire cells)
mgrs_30T_grid <- vect('data/MGRS grid/MGRS_1km_30T_unprojected/MGRS_1km_30T_unprojected.shp')
cell2keep <- mgrs_30T_grid %>% intersect(madrid) %>% as.data.frame() %>% dplyr::select(MGRS) %>% deframe()
mgrs_30T_grid <- mgrs_30T_grid %>% terra::subset(mgrs_30T_grid$MGRS%in%cell2keep)

mgrs_30S_grid <- vect('data/MGRS grid/MGRS_1km_30S_unprojected/MGRS_1km_30S_unprojected.shp')
cell2keep <- mgrs_30S_grid %>% intersect(madrid) %>% as.data.frame() %>% dplyr::select(MGRS) %>% deframe()
mgrs_30S_grid <- mgrs_30S_grid %>% terra::subset(mgrs_30S_grid$MGRS%in%cell2keep)

mgrs_grid <- rbind(mgrs_30T_grid, mgrs_30S_grid) # rm(mgrs_30T_grid, mgrs_30S_grid, cell2keep)

# when converting mgrs to latlon, the program returns the lower left corner of the mgrs grid. Let's get centroids
mgrs_centroids <- centroids(mgrs_grid) %>% as.data.frame(geom='XY') %>% dplyr::select(MGRS,x,y)

# change latlon in dataset, species and points outside Madrid will be removed
atlas_obs <- merge(atlas_obs[,c('species','mgrs')], mgrs_centroids, by.x='mgrs', by.y='MGRS', all.x=T) %>% na.omit()

# check
# atlas_pts <- vect(atlas_obs, geom=c('x','y')); crs(atlas_pts) <- 'epsg:4326'
# plot(mgrs_grid);points(atlas_pts, col='red')



# fix names to match gbif backbone
atlas_obs$species[atlas_obs$species=='Chaenomeles lagenaria'] <- 'Chaenomeles japonica'
atlas_obs$species[atlas_obs$species=='Prunus insititia'] <- 'Prunus domestica subsp. insititia'

atlas_taxonomy <- rgbif::name_backbone_checklist(name_data=unique(atlas_obs$species)) %>%
  dplyr::select(verbatim_name, usageKey, scientificName, order, family, genus)



# traits
traits <- read_excel("data/AtlasFloraAloctonaMadrid.xlsx", sheet='traits') %>%
  as.data.frame() %>%
  subset(species %in% unique(atlas_obs$species))



# save final list
atlas_data <- list()
atlas_data[['observations']] <- atlas_obs
atlas_data[['taxonomy']] <- atlas_taxonomy
atlas_data[['traits']] <- traits
atlas_data[['mgrs_centroids']] <- mgrs_centroids

# save data
save(atlas_data, file="results/atlas_data.RData")
# save grid
writeVector(mgrs_grid, 'results/mgrs_grid.shp', overwrite=TRUE)


