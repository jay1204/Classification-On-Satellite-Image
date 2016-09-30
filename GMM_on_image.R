#################################
# CSC 591/791 HW2(Q1)           #
# GMM Clustering                #
# Zexi Chen                     #
#################################

rm(list=ls(all=T))

library(mclust)
library(lattice)
library(MASS)
library(car)
library(flexclust)      # for kcca
library(rgdal)          # read GeoTiff images into R
library(ggplot2)         # plot the image


# read the image
img <- readGDAL("ilk-3b-1024.tif")
# the image has three bands 

# plot the image
#image(vec, col= grey(1:99/100), axes=TRUE)

#names(vec)
#str(img)
# img@data // to get the data

#plot(vec)

# Assign RGB channels to data frame
imgRGB <- data.frame(
  img[]
)

#names(imgRGB)

#,
#R = img@data$band1,
#G = img@data$band2,
#B = img@data$band3

#imgRGB[c("band1", "band2", "band3")] <- imgRGB[c("band1", "band2", "band3")]/255

# set the number of sampling.
sampleNum <- floor(0.03*nrow(imgRGB))

#ggplot(data = imgRGB, aes(x = x, y = y)) + 
#  geom_point(colour = rgb(imgRGB[c("band1", "band2", "band3")]))

samp <- sample(1:nrow(imgRGB), sampleNum,replace=FALSE)

samImg <- imgRGB[samp,]

start.time2 <- Sys.time()
# Do model based clustering
#
g = 1:10
GMM = Mclust(samImg[,c("band1","band2","band3")], G=g)
end.time2 <- Sys.time()
time.takenBuildModel <- end.time2-start.time2