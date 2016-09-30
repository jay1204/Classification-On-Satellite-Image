##################
# CSC591/791 hw3 #
# Zexi Chen      #
##################

rm(list=ls(all=T))

source("readData.R")
library(rgdal)      # read Geotiff images into R
library(ggplot2)    # plot the images

trainData = read.table(file = "ilk-tr-xy.txt",sep=",")
testData = read.table(file= "ilk-te-xy.txt",sep=",")

imgData = readGDAL("ilk-3b-1024.tif")

img <- data.frame(
  imgData[]
)

rangX <- 1024
trainNeighborSize <- 2

trainSet<- readData(img,trainData,rangX,trainNeighborSize)
trainSet["Label"] <- NA
for(i in seq(nrow(trainData))){
  for(j in 1:((trainNeighborSize*2+1)*(trainNeighborSize*2+1))){
    trainSet[(i-1)*(trainNeighborSize*2+1)*(trainNeighborSize*2+1)+j,c("Label")]<-trainData[i,4]
  }
}