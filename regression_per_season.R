#function that creates two new stacks with linear regression information. 
#It takes all .tif images from dir and makes a linear regression of all. 
#Images that the function takes must have one layer for each year. (num_years layers)
stack_regression <- function(dir,num_years){
  
  #Import necesary libraries. 
  library(sp)
  library(raster)
  library(maps)
  library(rgdal)
  library(grid)
  
  #set working directory and import images.
  setwd(dir)
  files <- list.files(pattern = "*.tif")
  print("Files ready")

  #create the stack with all files.  
  total_stack <- stack("mean_total.tif")
  
  #Replace all NA values with -9999
  print("Replacing NA values")
  total_stack[is.na(total_stack)]<- (-9999)
  time<- 1:num_years #create an array of all years we'll work with. 
  #create a function that calculate a regression and returns its coefficients as a matrix.
  regression_fun <- function(stack_evaluate){lm(stack_evaluate ~ time)$coefficients}
  coef_stack <- calc(total_stack,regression_fun)
  print("Regression done")
  writeRaster(coef_stack, filename = "regression_stack.tif", overwrite=TRUE)
  print("Finished writing file")
  return(coef_stack)
}

stack_regression("/home/florencia/Documentos/UC/NDVI_ZAA/stats",17)
#coefficients entrega 2 valores, X.Intercept y return_time. 
#dist = X.intercept + return_time*time 