

# CREATE MEAN RASTER OF NPP OVER THE LAST TWO DECADES FROM MODIS NPP PRODUCT


library(MODISTools)


EVI2 (productivity): 2002, 2006, 2012, 2018 [MODIS 500m: MCD12Q2]
View(mt_products())
View(mt_bands(product = "MYD17A3HGF"))
mt_dates(product = "MCD12Q1", lat = 40.395898, lon = -3.703260)
modis_sinusoidal <- '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs'

madrid_NPP <- mt_subset(product="MYD17A3HGF", band='Npp_500m',
                              start="2002-01-01", end="2020-12-31",
                              lat=40.495, lon=-3.71686,
                              km_lr=100, km_ab=100,
                              site_name="Madrid",
                              internal=TRUE, progress=TRUE)

madrid_NPP$value[madrid_NPP$value==32767] <- NA # remove outliers

# convert into raster and calculate mean
Madrid_NPP <- madrid_NPP %>% mt_to_terra(reproject=F) %>%  mean(na.rm=T)
names(Madrid_NPP) <- 'annual_productivity'

# remove outliers
Madrid_NPP[Madrid_NPP>1] <- 0
Madrid_NPP[is.na(Madrid_NPP)] <- 0

# reproject and plot
Madrid_NPP <- Madrid_NPP %>% project('epsg:4326')
plot(Madrid_NPP); lines(madrid)

# save raster
writeRaster(Madrid_NPP, file='results/Madrid_NPP.tiff')


