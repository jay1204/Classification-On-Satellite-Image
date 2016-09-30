readData <-function(image,data,rangeX,neighborSize){
  size<-2*neighborSize+1
  sample<-matrix(0,nrow(data)*(2*neighborSize+1)*(2*neighborSize+1),1)
  for(i in seq(nrow(data))){
    
    for(j in 1:size){
      for(k in 1:size){
        sample[(i-1)*size*size+k+(j-1)*size,1]<- data[i,3]*rangeX+data[i,2]- (neighborSize-j+1)*rangeX-neighborSize + k
      }
    }
  }
  
  extractImg <- image[sample,]
  extractImg
}