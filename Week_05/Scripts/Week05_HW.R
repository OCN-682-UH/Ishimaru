# MBIO 612: Week 5B
# Created by: Shelbie Ishimaru
# Created on: 2024-09-24
################################################################################
# Load Libraries ---------------------------------------------------------------
library(tidyverse) #for data manipulation
library(here) #for unbreakable file paths
library(lubridate) #package to deal with dates and times
library(cowsay) #fun package of the day!

# Read-in Data -----------------------------------------------------------------
CondData<-read_csv(here("Week_05","data", "CondData.csv")) #Load cond data
glimpse(CondData) #look at data

DepthData<-read_csv(here("Week_05","data", "DepthData.csv")) #Load depth data
glimpse(DepthData) #look at data

# Data Analysis ----------------------------------------------------------------
cond_depth <- CondData %>% 
  mutate(date= mdy_hms(date)) %>% #create an new column that converts the date column to datetime
  mutate(date= floor_date(date, unit= "10 seconds")) %>% #round datetimes to nearest 10 seconds to match the depth data
  inner_join(DepthData) %>% #join the cond with depth data, key= "date" datetime col
  select(date, Temperature, Salinity, Depth) %>% #select specific cols for analysis
  mutate(date= floor_date(date, unit= "minute")) %>% #round datetimes to the nearest minute
  pivot_longer(cols = Temperature:Depth, #the cols you want to pivot. This says select the temp, silicate, and pH cols
               names_to = "Variables", #the names of the new cols with all the column names
               values_to = "Values") %>% #names of the new column with all the values
  group_by(date, Variables) %>% #group by datetime to obtain mean by minute
  summarise(mean_vals= mean(Values, na.rm = T)) %>% #calculate mean by minute for temp, serial, and salinity, gives warning message but calcualtes correctly
  ggplot(aes(x= date, y= mean_vals)) + geom_line() + #create line plot with time on x and mean vals on the y
  facet_wrap(~Variables, scales= "free_y") + #facet_wrap so each variable has its own plot
  #NOTE: I did not alter facet_wrapped variable labels because I could not find a data dictionary and did not want to assume the units for each variable
  labs(x= "Time", #x-axis label
       y= "Mean Values", #y-axis label
       title= "Changes in Depth, Salinity, and Temperature on 1/15/2021") + #plot title 
  theme_bw() + #change theme so we have a white background with grey grid lines and black outline
  theme(axis.title= element_text(size= 11), #change axes text size
        plot.title= element_text(hjust= 0.5, #change title text size
                                 face= "bold")) + #make title bold
  ggsave(here("Week_05", "Outputs", "Homework_plot.png"), #saves the ggplot using an unbreakable file path
         width= 12, height= 5) #gives desired plot dimensions
#NOTE: I get a error with ggsave() but it still saves the final plot to the correct file path

# Quick Celebration :) ---------------------------------------------------------
say("Awesome Job!", by= "whale")
