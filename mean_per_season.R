#otra idea es calcular todos los promedios por año y season en un solo stack con la funcion stackApply
#despues dividir ese stack segun las estaciones a nuevos stacks. 

#Import necesary libraries. 
library(sp)
library(raster)
library(maps)
library(rgdal)
library(grid)


#return a list with 4 stacks, each stack represent a season (summer,autumn,winter,spring)
#Each 5 or 6 layers represent a year. 
#File names must be in order and start in the first year to read. 
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
  print(paste("summer",files[5]))
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
    print(paste("summer",files[5]))
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


#method that calculates the average per season 
#it generates a new file for each season with the average of each year. 
mean_by_season <- function(dir,num_years){
  
  #we separate data for each season. 
  stacks <- separate_by_season(dir,num_years)
  summer_stack <- stacks$summer
  autumn_stack <- stacks$autumn
  winter_stack <- stacks$winter
  spring_stack <- stacks$spring
  
  #create an array with index distribution to do stackAppy
  #For summer we choose 5 images to make a year. 
  indexes_fin <- c()
  for (i in 1:num_years) {
    indexes <- rep(i,times=5)
    indexes_fin <- c(indexes_fin,indexes)
  }
  print("calculating mean for summer")
  stackApply(summer_stack,indices = indexes_fin ,fun=mean,filename = 'mean_summer.tif')
  
  
  #create an array with index distribution to do stackAppy
  #For autumn,winter,spring we choose 6 images to make a year. 
  for (i in 1:num_years) {
    indexes <- rep(i,times=6)
    indexes_fin <- c(indexes_fin,indexes)
  }
  
  print("calculating mean for autumn")
  stackApply(autumn_stack,indices = indexes_fin ,fun=mean,filename = 'mean_autumn.tif')
  
  print("calculating mean for winter")
  stackApply(winter_stack,indices = indexes_fin ,fun=mean,filename = 'mean_winter.tif')
  
  print("calculating mean for spring")
  stackApply(spring_stack,indices = indexes_fin ,fun=mean,filename = 'mean_spring.tif')
  
}

