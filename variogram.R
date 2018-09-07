library(sp)
library(raster)
library(maps)
library(rgdal)
library(grid)
library(usdm)

setwd("/home/florencia/Documentos/UC/NDVI_ZAA/stats")
ls()

my_stack <- stack("mean_total.tif")

#calculate variogram for each layer
variograms_final <- c()
for (layer in 1:nlayers(my_stack)) {
  print(paste("working on layer",layer))
  new_variogram<-Variogram(my_stack[[layer]])
  variograms_final<-c(variograms_final,new_variogram)
}

#crear matriz con distancias y gamma1
final_matrix<-c()
final_matrix<- variograms_final[[1]]@variogram

#append new columns to final_matrix. 
for (i in 2:17) {
  colname<- paste("col",i,sep="")
  assign(colname,colu)
  colu <- variograms_final[[i]]@variogram[,2]
  final_matrix<-cbind(final_matrix,colu)
}

#chage column names to make sense
colnames(final_matrix)<-c("distance","gamma1","gamma2","gamma3","gamma4","gamma5","gamma6","gamma7","gamma8","gamma9","gamma10","gamma11","gamma12","gamma13","gamma14","gamma15","gamma16","gamma17")
final_matrix["distance"]<- final_matrix["distance"]*111000 #transform distances to meters
write.csv(final_matrix,file="gamma_matrix_meantotal.csv")
