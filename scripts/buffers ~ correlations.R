

# CHECK CORRELATION AMONG PREDICTORS AT DIFFERENT BUFFERS


library(tidyverse)


# import data
pred_0.5km <- read.csv("results/pred_0.5km.txt", sep="")
pred_1km <- read.csv("results/pred_1km.txt", sep="")
pred_2.5km <- read.csv("results/pred_2.5km.txt", sep="")
pred_5km <- read.csv("results/pred_5km.txt", sep="")


# stream density
x_buff <- data.frame(x0.5 = pred_0.5km$LCF5, x1 = pred_1km$LCF5,
                     x2.5 = pred_2.5km$LCF5, x5 = pred_5km$LCF5)

pairs(x_buff,lower.panel=NULL)
cor(x_buff)
