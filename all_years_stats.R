#probar para 2, 5, 10, 17 años cuanto demora aggregate y cuanto demora math con "split"
library(sp)
library(raster)
library(maps)
library(rgdal)
library(grid)

setwd("/home/florencia/Documentos/UC/NDVI_ZAA/")
ls()

aggregate_test<- function(){

  files <- list.files(pattern = "*.tif")
  print("Files ready")
  
  #create the array to loop on 
  anos <- c(2,5,10,17)
  
  #we do the experiment for different years
  for(ano in anos) {
    print(paste("Haciendo el calculo para",ano,"años"))
    cant_imagenes <- ano*23
    
    #calculate mean for each year and show how long does it take to do it. 
    stack1 <- stack(files[1:cant_imagenes])
    #aggregate
    print("Calculamos con aggregate")
    ti_agg <- Sys.time()
    stack_final <- aggregate(stack1,fact = c(1,1,23),fun=var)
    tf_agg <- Sys.time()
    tt_agg <- tf_agg - ti_agg
    
    print(paste("Para", ano,"años, aggregate toma"))
    print(tt_agg)
  }
}

aggregate_test()

math_mean_test<-function(){
  files <- list.files(pattern = "*.tif")
  print("Files ready")
  
  #create the array to loop on 
  anos <- c(2,5,10,17)
  
  #we do the experiment for different years
  for(ano in anos) {
    print(paste("Haciendo el calculo para",ano,"años"))
    cant_imagenes <- ano*23
    
    #calculate mean for each year and show how long does it take to do it. 
    stack1 <- stack(files[1:cant_imagenes])
    #math_mean
    #calculamos los indices a juntar
    indices_fin <- c()
   
    print("Calculamos con stackaply")
    ti_math <- Sys.time()
    for (i in 1:ano) {
      indices <- rep(i,times=23)
      indices_fin <- c(indices_fin,indices)
    }
    stackApply(stack1,indices = indices_fin ,fun=var)
    tf_math <- Sys.time()
    tt_math <- tf_math - ti_math
    
    print(paste("Para", ano,"años, math toma"))
    print(tt_math)
  }
}

math_mean_test()

