# MBIO 612: Week 13A
# Created by: Shelbie Ishimaru
# Created on: 2024-12-3
################################################################################
# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(here)

# Simple For Loop --------------------------------------------------------------
#Pre-allocate space for the for loop
#empty matrix that is as long as the years vector
year_data<-tibble(year =  rep(NA, length(years)),  # column name for year
                  year_name = rep(NA, length(years))) # column name for the year name

years <- c(2015:2021) #create a sequence

for (i in 1:length(years)){ # set up the for loop where i is the index
  year_data$year_name[i]<-paste("The year is", years[i]) # loop over i
  year_data$year[i]<-years[i] # loop over year
}

year_data

# Using Loops to Read in CSV Files ---------------------------------------------
#point to the location on the computer of the folder
CondPath <- here("Week_13", "Data", "cond_data")
#list all the files in that path with a specific pattern
#In this case we are looking for everything that has a .csv in the filename
#you can use regex to be more specific if you are looking for certain patterns in filenames
files <- dir(path= CondPath,pattern= ".csv")
files

# Calculate the Mean, Salinity, and Temp From Each File and Save it ------------
#pre-allocate space
#make an empty dataframe that has one row for each file and 3 columns
cond_data<-tibble(filename =  rep(NA, length(files)),  #column name for year
                  mean_temp = rep(NA, length(files)), #column name for the mean temperature
                  mean_sal = rep(NA, length(files)), #column name for the mean salinity
) #column name for the year name
cond_data

#create the for loop
for (i in 1:length(files)){ # loop over 1:3 the number of files 
  raw_data<-read_csv(paste0(CondPath,"/",files[i]))
  #glimpse(raw_data)
  cond_data$filename[i]<-files[i]
  cond_data$mean_temp[i]<-mean(raw_data$Temperature, na.rm =TRUE)
  cond_data$mean_sal[i]<-mean(raw_data$Salinity, na.rm =TRUE)
} 
cond_data

# purrr ------------------------------------------------------------------------
#Simple Example
#1. Use a canned function that already exists
1:10 %>% # a vector from 1 to 10 (we are going to do this 10 times) %>% # the vector to iterate over
  map(rnorm, n = 15)  %>% # calculate 15 random numbers based on a normal distribution in a list 
  map_dbl(mean) # calculate the mean. It is now a vector which is type "double"

#2. Make your own function
1:10 %>% # list 1:10
  map(function(x) rnorm(15, x)) %>% # make your own function
  map_dbl(mean)

#3. Use a formula when you want to change the arguments within the function
1:10 %>%
  map(~ rnorm(15, .x)) %>% # changes the arguments inside the function
  map_dbl(mean)

# Using purrr to Read in CSV Files 
files <- dir(path= CondPath,pattern= ".csv", full.names= T)
data<-files %>% # files is what we are iterating over
  set_names() %>% # set's the id of each list to the file name
  map_df(read_csv,.id = "filename")  %>% # map everything to a dataframe and put the id in a column called filename
  group_by(filename) %>%
  summarise(mean_temp = mean(Temperature, na.rm = TRUE),
            mean_sal = mean(Salinity,na.rm = TRUE))
data
