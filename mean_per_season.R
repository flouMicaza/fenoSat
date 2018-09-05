#otra idea es calcular todos los promedios por año y season en un solo stack con la funcion stackApply
#despues dividir ese stack segun las estaciones a nuevos stacks. 

#Import necesary libraries. 
library(sp)
library(raster)
library(maps)
library(rgdal)
library(grid)


#method that calculate mean for each season of the years in the folder.
#It creates a new file for each season.
#Each layer of the new file has the mean value of a year in that season. 
#File names must be in order and start in the first year to read. 
mean_per_season <- function(dir, num_years){
  #set directory and read files. 
  setwd(dir)
  files <- list.files(pattern = "*.tif")
  print("Files ready")
  
  num_images <- 23*num_years #calculate how many images to read.

  #we create stacks for each season with the first year images (this is to avoid working with empty stacks)
  #For summer season a year is represented by 5 images
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
  
  
}
