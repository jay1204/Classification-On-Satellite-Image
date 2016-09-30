##################
# CSC591/791 hw3 #
# Zexi Chen      #
##################

rm(list=ls(all=T))

source("readData.R")
library(rgdal)      # read Geotiff images into R
library(ggplot2)    # plot the images
library(e1071)      # build the bayesian model(naiveBayes) and SVM(svm) 
library(rpart)      # build the decision tree model
library(MASS)
library("neuralnet")

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

testSet <- readData(img,testData,rangX,0)
testSet["Baye"]<-NA
testSet["DT"]<-NA
testSet["Label"]<-NA
testSet["Neural"]<-NA
testSet["SVM"]<-NA

formula <- Label ~ band1 + band2 + band3
baye <- naiveBayes(formula,data=trainSet,method = "class")
testBaye <- predict(baye,testSet[,c("band1","band2","band3")],type = "raw")

Dtree <- rpart(formula,data=trainSet,method="class")
testDtree <- predict(Dtree,testSet[,c("band1","band2","band3")])

neural<-neuralnet(formula,data=trainSet)
testNeural<- predict(Dtree,testSet[,c("band1","band2","band3")])

svmModel<-svm(formula,data=trainSet,type = "C")
testSVM<- predict(svmModel,testSet[,c("band1","band2","band3")])

for(i in 1:nrow(testSet)){
  testSet[i,c("Label")]<-testData[i,4]
  testSet[i,c("Baye")]<-which(testBaye[i,]==max(testBaye[i,]))
  testSet[i,c("DT")]<-which(testDtree[i,]==max(testDtree[i,]))
  testSet[i,c("Neural")]<-which(testDtree[i,]==max(testNeural[i,]))
  testSet[i,c("SVM")]<-testSVM[i]
}

write.csv(testSet,file="q1(a)_result")

