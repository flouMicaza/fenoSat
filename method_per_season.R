#Import necesary libraries. 
library(sp)
library(raster)
library(maps)
library(rgdal)
library(grid)

###########################SEPARATE BY SEASON ###############################################################
#Return a list with 4 stacks, each stack represent a season (summer,autumn,winter,spring)
#Each 5 or 6 layers represent a year. 
#File names in dir must be in order and start in the first year to read. 
separate_by_season <- function(dir, num_years){
  #set directory and read files. 
  setwd(dir)
  files <- list.files(pattern = "*.tif")
  print("Files ready")
  
  num_images <- 23*num_years #calculate how many images to read.
  
  print("Starting to divide images for each season")
  #we create stacks for each season with the first year images (this is to avoid working with empty stacks)
  #For summer season a year is represented by 5 images
  print("Year 1")
  images<-1:5
  
  
  summer_stack <-stack(files[1:5]) #append new layers
  files <- files[-images] #drop images that are already in a stack. 
  
  #For other seasons a year is represented by 6 images. 
  images<-1:6
  autumn_stack <- stack(files[1:6]) #append new layers
  files <- files[-images]
  
  winter_stack <- stack(files[1:6]) #append new layers
  files <- files[-images]
  
  spring_stack <-stack(files[1:6]) #append new layers
  files <- files[-images]
  
  
  #We add the images that represent a year for each season. 
  for (year in 2:num_years) {
    
    print(paste("Year",year))
    
    #For summer season a year is represented by 5 images
    
    images<-1:5
    summer_stack <- stack(summer_stack,stack(files[1:5])) #append new layers
    files <- files[-images] #drop images that are already in a stack. 
    
    #For other seasons a year is represented by 6 images. 
    images<-1:6
    autumn_stack <- stack(autumn_stack,stack(files[1:6])) #append new layers
    files <- files[-images]
    
    winter_stack <- stack(winter_stack,stack(files[1:6])) #append new layers
    files <- files[-images]
    
    spring_stack <- stack(spring_stack,stack(files[1:6])) #append new layers
    files <- files[-images]
  }
  
  results <- list("summer"=summer_stack,"autumn"=autumn_stack,"winter"=winter_stack,"spring"=spring_stack)
  print("Stacks per season ready!")
  return(results)
  
}

###########################MEAN BY SEASON ###################################################################
#Method that calculates a method per season 
#It generates a new file for each season with the result of the method of each year if export=TRUE.
#It also generates a file with mean values per year and spring-autumn values. 
#Return a list with 4 stacks, each stack represent a year. 
calc_by_season <- function(dir,num_years,method,filename,export=TRUE){
  
  #we separate data for each season. 
  stacks <- separate_by_season(dir,num_years)
  summer_stack <- stacks$summer
  autumn_stack <- stacks$autumn
  winter_stack <- stacks$winter
  spring_stack <- stacks$spring
  
  #Create a new folder to save new files. 
  #If it already exists it will show a warning. 
  new_dir<- paste(dir,"/stats",sep="")
  dir.create(new_dir)  
  setwd(new_dir)
  
  
  #Create an array with index distribution to do stackAppy
  #For summer we choose 5 images to make a year.
  indexes_fin <- c()
  for (i in 1:num_years) {
    indexes <- rep(i,times=5)
    indexes_fin <- c(indexes_fin,indexes)
  }
  
  print("calculating mean for summer")
  final_summer<- stackApply(summer_stack,indices = indexes_fin ,fun=method)
  
  
  #create an array with index distribution to do stackAppy
  #For autumn,winter,spring we choose 6 images to make a year. 
  indexes_fin<-c()
  for (i in 1:num_years) {
    indexes <- rep(i,times=6)
    indexes_fin <- c(indexes_fin,indexes)
  }
  
  print("calculating mean for autumn")
  final_autumn<- stackApply(autumn_stack,indices = indexes_fin ,fun=method)
  
  
  print("calculating mean for winter")
  final_winter <-stackApply(winter_stack,indices = indexes_fin ,fun=method)
  
  print("calculating mean for spring")
  final_spring <- stackApply(spring_stack,indices = indexes_fin ,fun=method)
  
  
  
  #return results in case we need them later. 
  results <- list("summer"=final_summer,"autumn"=final_autumn,"winter"=final_winter,"spring"=final_spring)
  
  
  #calculate the average of all years. Creates a TIF file with one layer. 
  indexes_fin <- c()
  indexes_fin <- 1:num_years
  indexes_fin<- c(indexes_fin,indexes_fin,indexes_fin,indexes_fin)
  print("calculating mean per year")
  final_stack <- stack(final_summer,final_autumn,final_winter,final_spring)
  final_mean <- stackApply(final_stack,indices =indexes_fin, fun = method)
  
  #calculate a delta of spring and autumn. 
  print("calculating delta spring-autumn")
  final_delta <- autumn_stack-spring_stack
  print("Mean calculated!")
  
  #create files only if export = TRUE
  if(export){
    print("writing files")
    
    file_name<- paste(filename,"_autumn.tif",sep="")
    writeRaster(final_spring, filename = file_name, overwrite=TRUE)
    
    file_name<- paste(filename,"_summer.tif",sep="")
    writeRaster(final_winter, filename = file_name, overwrite=TRUE)
    
    file_name<- paste(filename,"_spring.tif",sep="")
    writeRaster(final_autumn, filename = file_name, overwrite=TRUE)
    
    file_name<- paste(filename,"_winter.tif",sep="")
    writeRaster(final_summer, filename = file_name, overwrite=TRUE)
    
    file_name<- paste(filename,"_total.tif",sep="")
    writeRaster(final_mean, filename = file_name, overwrite=TRUE)
    
    file_name<- paste(filename,"_delta_spring_autumn.tif")
    writeRaster(final_delta,filename = file_name,overwrite = TRUE)
  }
  
  return(results)
}


#############################################################################################################
calc_by_season("/home/florencia/Documentos/UC/NDVI_ZAA",17,sd,"SD")
