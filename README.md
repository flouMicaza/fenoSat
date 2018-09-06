# fenoSat
Fenología de satélites

## all_years_stats.R
File that takes 17 years of images and compares how long does 3 different methods take to evaluate the average of a year. 

## one_year_stats.R 
File that takes one year of images and compares how long does 3 different methods take to evaluate the average of a year. 
## final_script.R
Script that calculates the average of 17 years of images and calculates t_value and p_value of a linear regression made on the output of average scripts. 

## mean_per_season.R
There is two methods in this script: 
* separate_by_season(dir,num_years): it takes all of the images found in dir and make new stacks with the information of each season. Returns a list of stacks. 
* mean_by_season: it calculates the average for each year in each season stack. 
There is the option of writing a new file for each stack or dont with the export flag. 

## method_per_season.R 
Same script as mean_per_season but in method_by_season method you can choose which method to aplly to the files. It must be a method that return only one value. 

## regression_per_season.R
calculates the linear regression for a stack that cotains num_years years and creates a new stack with t_value or p_palue. 
