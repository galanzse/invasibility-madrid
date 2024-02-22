

# GET INFO ON NATIVE RANGE OF EXOTIC SPECIES TO UNDERSTAND CLIMATIC CARACTERISTICS IN RECIPIENT ARE


library(tidyverse)
library(rWCVP)
# remotes::install_github('matildabrown/rWCVPdata')
library(rWCVPdata)


# import data
load("C:/Users/user/OneDrive/ACADEMICO/proyectos/ailanthus/results/atlas_data.RData")
mgrs_pred_1km <- read.csv("results/mgrs_pred_1km.txt", sep="")


# base map
occ_map_native <- rast('C:/Users/user/Desktop/worldclim/wc2.1_2.5m_elev/wc2.1_2.5m_elev.tif')
occ_map_native[] <- NA

# extract native ranges
occ_xy_native <- list()
for (i in 88:nrow(atlas_data[['traits']])) {
  
  # download data, rasterize and get centroids of 4.5km2 cells in native range, unprojected
  temp <- wcvp_distribution(atlas_data[['traits']]$species_wcvp[i], taxon_rank="species",
                            native=T, introduced=F, extinct=T, location_doubtful=F,
                            wcvp_names = NULL) %>%
    vect() %>%
    rasterize(y=occ_map, field='occurrence_type', background=NA) %>%
    as.data.frame(xy=T)
  
  # add name and store in list
  temp$species <- atlas_data[['taxonomy']]$verbatim_name[i]
  occ_xy_native[[atlas_data[['taxonomy']]$verbatim_name[i]]] <- temp
  
  # progress
  print(paste(round(i/nrow(atlas_data[['taxonomy']])*100, 2), '% -> ', atlas_data[['taxonomy']]$verbatim_name[i], sep=''))

}

# save data
save(occ_xy_native, file="results/occ_xy_native.RData")


