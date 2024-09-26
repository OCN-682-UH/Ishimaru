# MBIO 612: Week 5A
# Created by: Shelbie Ishimaru
# Created on: 2024-09-24
################################################################################
# Load Libraries ---------------------------------------------------------------
library(tidyverse)
library(here)
library(cowsay) #fun package of the day!

# Load Data ---------------------------------------------------------------
#Environmental data from each site
EnviroData<-read_csv(here("Week_05","data", "site.characteristics.data.csv"))

#Thermal performance data
TPCData<-read_csv(here("Week_05","data","Topt_data.csv"))

#look at data
glimpse(EnviroData)
glimpse(TPCData)

#Pivot the data so both dfs are in the same format
EnviroData_wide <- EnviroData %>% #make environmental data long to match the coral
  pivot_wider(names_from = parameter.measured,
              values_from = values)
View(EnviroData_wide) #look at pivoted data

#arrange the data so that the site letter col is alphabetical 
EnviroData_wide <- EnviroData %>% 
  pivot_wider(names_from = parameter.measured, # pivot the data wider
              values_from = values) %>%
  arrange(site.letter) # arrange the df by site
View(EnviroData_wide) #look at sorted data

#Left join: join the coral data with the environmental data
FullData_left<- left_join(TPCData, EnviroData_wide) #joining with by = join_by(site.letter), dont need to specify because its coded correctly 
head(FullData_left) #look at joined data

#relocate so site metadata comes before the numerical data
FullData_left<- left_join(TPCData, EnviroData_wide) %>% #joining with by = join_by(site.letter), dont need to specify because its coded correctly 
  relocate(where(is.numeric), .after = where(is.character)) # relocate all the numeric data after the character data, where is numeric and put it first
head(FullData_left) #look at joined and relocated data

#Calculate mean and variance for all collected data
calculated_data <- FullData_left %>% 
  group_by(site.letter) %>%
  summarise_at(vars(E:Topt, light:substrate.cover), funs(mean= mean, var= var), na.rm = TRUE)

#Create a tibble
T1 <- tibble(Site.ID = c("A", "B", "C", "D"), #example that has missing data
             Temperature = c(14.1, 16.7, 15.3, 12.8))
T1 #view tibble 1

T2 <-tibble(Site.ID = c("A", "B", "D", "E"), #example that has missing data
            pH = c(7.3, 7.8, 8.1, 7.9))

T2 #view tibble 2

#Practice joining with T1 and T2
left_join(T1, T2) #joins T2 to T1, excludes site E, includes NAs for missing data
right_join(T1, T2) #joins T1 to T2, excludes site C, includes NAs for missing data
inner_join(T1, T2) #removes site C and E because they are not complete data sets
full_join(T1, T2) #keeps everything and replaces missing data with NAs
semi_join(T1, T2) #keeps all rows from the first df that has matching data to the second df
anti_join(T1, T2) #saves rows from the first df that are not found in the second df

#Cowsay 
say("Hello", by= "whale")
say("Hello", by= "random")
