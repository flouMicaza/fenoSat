
########################## P_VALUE REGRESSION ###############################################################

#Method that creates 5 new stacks with pvalue of linear regression. 
#It takes all .tif images from dir and makes a linear regression of all. 
#Images that the function take must have one layer for each year. (num_years layers)
pvalue_regression <- function(dir,num_years){
  
  #Import necesary libraries. 
  library(sp)
  library(raster)
  library(maps)
  library(rgdal)
  library(grid)
  
  #Set working directory and import images.
  setwd(dir)
  files <- list.files(pattern = "*.tif")
  print("Files ready")

  #create the stack with all files.  
  total_stack <- stack("mean_total.tif")
  summer_stack <- stack("mean_summer.tif")
  winter_stack <- stack("mean_winter.tif")
  autumn_stack <- stack("mean_autumn.tif")
  spring_stack <- stack("mean_spring.tif")
  delta_stack <- stack("delta_spring_autumn.tif")
  
  #Replace all NA values with -9999
  print("Replacing NA values")
  print("Replacing NA values total")
  total_stack[is.na(total_stack)]<- (-9999)
  #summer_stack[is.na(summer_stack)]<- (-9999)
  #winter_stack[is.na(winter_stack)]<- (-9999)
  print("Replacing NA values autumn")
  autumn_stack[is.na(autumn_stack)]<- (-9999)
  print("Replacing NA values spring")
  spring_stack[is.na(spring_stack)]<- (-9999)
  print("Replacing NA values delta")
  #delta_stack[is.na(delta_stack)]<- (-9999)
  
  time<- 1:num_years #create an array of all years we'll work with. 
  
  #create a function that calculate a regression and returns Pr(>|t|)
  pvalue_fun <- function(stack_evaluate){summary(lm(stack_evaluate ~ time))$coefficients[2,4]}
  
  #create list with all stacks
  stacks <- c("total"=total_stack,"summer"=summer_stack,"winter"=winter_stack,"autumn"=autumn_stack,"spring"=spring_stack)
  #create a list with all stack names
  name_stacks <- c("total","autumn","spring")
  return_stacks <- c()
  #calculate pvalue for each stack
  for (stack in name_stacks) {
    print(paste("calculating p_value for",stack))
    file_name <- paste(stack,"_",'pvalue.tif',sep="")
    stack_to_calc<- stacks[[stack]]
    calc(stack_to_calc,pvalue_fun,filename=file_name,overwrite=TRUE)
  }
  
  print("Regression done")
  return(return_stacks)
}

########################### T_VALUE REGRESSION ##############################################################

#Function that creates 4 new stacks with tvalue for each pixel. 
#It takes all .tif images from dir and makes a linear regression of it. 
#Images that the function takes must have one layer for each year. (num_years layers)
tvalue_regression <- function(dir,num_years,export=TRUE){
  
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
  summer_stack <- stack("mean_summer.tif")
  winter_stack <- stack("mean_winter.tif")
  autumn_stack <- stack("mean_autumn.tif")
  spring_stack <- stack("mean_spring.tif")
  delta_stack <- stack("delta_spring_autumn.tif")
  #Replace all NA values with -9999
  print("Replacing NA values")
  print("Replacing NA values total")
  total_stack[is.na(total_stack)]<- (-9999)
  #print("Replacing NA values summer")
  #summer_stack[is.na(summer_stack)]<- (-9999)
  #print("Replacing NA values winter")
  #winter_stack[is.na(winter_stack)]<- (-9999)
  print("Replacing NA values autumn")
  autumn_stack[is.na(autumn_stack)]<- (-9999)
  print("Replacing NA values spring")
  spring_stack[is.na(spring_stack)]<- (-9999)
  print("Replacing NA values deta")
  #delta_stack[is.na(delta_stack)]<- (-9999)
  time<- 1:num_years #create an array of all years we'll work with. 

  #create a function that calculate a regression and returns t value
  tvalue_fun <- function(stack_evaluate){summary(lm(stack_evaluate ~ time))$coefficients[2,3]}
  
  #create a list of all stacks
  stacks <- c("total"=total_stack,"autumn"=autumn_stack,"spring"=spring_stack)
  #create a list of all stack names
  name_stacks <- c("total","autumn","spring")
  return_stacks <- c()
  #calculate for each stack the tvalue of a linear regression.
  for (stack in name_stacks) {
    print(paste("calculating t_value for",stack))
    file_name <- paste(stack,"_",'tvalue.tif',sep="")
    stack_to_calc<- stacks[[stack]]
    calc(stack_to_calc,tvalue_fun,filename=file_name,overwrite=TRUE)
  }
  
  print("Regression done")
  
  
  return(coef_stack)
}

###################################################################################################

tval<-tvalue_regression("/home/florencia/Documentos/UC/NDVI_ZAA/stats/Flo/stats",17)

pval <-pvalue_regression("/home/florencia/Documentos/UC/NDVI_ZAA/stats/Flo/stats",17)

#In summary.lm(lm(stack_evaluate ~ time)) :
#essentially perfect fit: summary may be unreliable

#calcular para la media de cada estacion, sacar el p value y el t value. 