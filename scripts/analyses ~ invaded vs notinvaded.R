

# COMPARE ATTRIBUTES OF OCCUPIED VS NON-OCCUPIED CELLS


library(tidyverse)
library(ggpubr)
# library(terra)



# import data
load("results/atlas_data.RData")
pred_2.5km <- read.csv("results/pred_2.5km.txt", sep="")
table(is.na(pred_2.5km))

# variables of interest
predictors <- colnames(pred_2.5km)[-which(colnames(pred_2.5km)%in%c('MGRS','x','y'))]

# add info occupied/notoccupied
pred_2.5km$occupied <- pred_2.5km$MGRS %in% atlas_data$observations$mgrs
table(pred_2.5km$occupied)

# df in long format
pred_2.5km_long <- pred_2.5km %>% pivot_longer(all_of(predictors))
pred_2.5km_long$occupied <- as.factor(pred_2.5km_long$occupied)
levels(pred_2.5km_long$occupied) <- c('unoccupied','occupied')
head(pred_2.5km_long)

# plot occupied vs notoccupied
par(mfrow=c(3,5))
for (i in predictors) {
  temp <- pred_2.5km_long %>% dplyr::filter(name==i)
  boxplot(value ~ occupied, data=temp, main=paste(i,'2.5km'), xlab='', ylab='')
  abline(a=median(temp$value[temp$occupied=='unoccupied']),b=0,lty=2,col='red')
}



# climate variables indicate geographical differences, plot density of observations
ggplot(aes(x=x, y=y, color=occupied), data=pred_2.5km[pred_2.5km$occupied=='TRUE',]) +
  geom_density2d_filled()

g1 <- ggplot(aes(x=x, y=y, color=occupied), data=pred_2.5km[pred_2.5km$occupied=='FALSE',]) +
  stat_density_2d(geom="polygon", contour=T, aes(fill=after_stat(level)), colour="black", bins=10) +
  scale_fill_distiller(palette="Blues", direction=1) +
  theme_classic() + ggtitle('Density unoccupied cells')

g2 <- ggplot(aes(x=x, y=y, color=occupied), data=pred_2.5km[pred_2.5km$occupied=='TRUE',]) +
  stat_density_2d(geom="polygon", contour=T, aes(fill=after_stat(level)), colour="black", bins=10) +
  scale_fill_distiller(palette="OrRd", direction=1) +
  theme_classic() + ggtitle('Density occupied cells')

ggarrange(g1, g2, ncol=2)


