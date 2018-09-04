#import libraries 
library(sp)
library(raster)
library(maps)
library(rgdal)
library(grid)

#set working directory
#In this directory you must have the tif files that will be used. 
setwd("/home/florencia/Documentos/UC/NDVI_ZAA/Archivos")
ls()

#We read all files in the folder setted before and create a stack. 
files <- list.files(pattern = "*.tif")
print("Files ready")


#Calculate stack mean for each cell and create new file. 
#We'll make an experiment and see what takes less time : calc,math method or aggregate. 
#aggregate crea un nuevo rasterLayer pero con menos dimensiones.

#Probar para un solo año, 3 años 7 años, 11 años y 17 años y para todos. 

#creo el stack para un año (23 imagenes)
anos <- 1
print(paste("Create stack for",anos,"years"))
cant_img0 <- 24
cant_img1 <- 23*2
#var_name <- paste("stack",anos)
var_name<-"stack1"
assign(var_name,stack(files[cant_img0:cant_img1])) #creación del stack con la cantidad de imagenes segun la cant de años a considerar. 

#Make experiment with calc #####################################
#Get initial time
ti_calc <- Sys.time()
mean_calc <-calc(stack1,mean) #calcular el promedio del stack i 
#Get final time
tf_calc<- Sys.time()
#calculate time it took to operate
total_time_calc<- tf_calc-ti_calc


#Make experiment with math ##################################
#Get initial time
ti_math <- Sys.time()
mean_math<-mean(stack1) #calcular el promedio del stack i
#Get final time
tf_math<- Sys.time()
#calculate time it took to operate
total_time_math<- tf_math-ti_math




#Make experiment with aggregate ##################################
#Get initial time
ti_agg <- Sys.time()
mean_agg <-aggregate(stack1,fact = c(1,1,23),fun=mean) #aggregate en profundidad por ano.
#Get final time
tf_agg<- Sys.time()
#calculate time it took to operate
total_time_agg<- tf_agg-ti_agg

print(paste("Resultados stack de 1 año"))
print(paste("It took", total_time_calc, "calculating mean with calc"))
print(paste("It took", total_time_math, "calculating mean with math method"))
print(paste("It took", total_time_math, "calculating mean with aggregate"))


