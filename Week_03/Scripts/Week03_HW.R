# MBIO 612: Week 3 Homework
# This script will create a bar plot that conveys how body mass (g) of 3 species of penguins differs by sex
# Created by: Shelbie Ishimaru
# Created on: 2024-09-14
################################################################################
# Load Libraries ---------------------------------------------------------------
library(palmerpenguins) #so we can load in our penguin data
library(tidyverse) #for data manipulation
library(dplyr) #for data manipulation
library(ggplot2) #for plotting
library(here) #used to create unbreakable file paths
library(LaCroixColoR) #a fun color palette!
library(ggthemes) #allows us to edit the plot theme
library(praise) #for motivation/celebration!

# Read-in Data -----------------------------------------------------------------
glimpse(penguins) #gives us a column overview, shows first 10 lines of df

# Data Analysis: Calculations --------------------------------------------------
#In this section I will calculate mean and standard deviation for error bars!
penguins_calc <- penguins %>% #create a new df to save final results and call the df we want to work with
  na.omit(penguins) %>% #remove NAs from the df so we can calculate mean and sd (NOTE: removes 11 individuals)
  group_by(species, sex) %>% #choose the columns we want mean and sd to be calculated for 
  summarise(body_mass= mean(body_mass_g), #calculate the mean of body mass (g) for each species and sex
            sd= sd(body_mass_g)) #calculate the standard deviation of body mass (g) for each species and sex

glimpse(penguins_calc) #look at our new df with calculations

# Data Analysis: Plotting ------------------------------------------------------
ggplot(data= penguins_calc, #call in new df with mean and sd calculations 
       mapping= aes(x= species, #x-axis is the different penguin species by sex
                    y= body_mass, #y-axis is the mean body mass for each species by sex
                    fill= sex)) + #color the bars by sex
  geom_bar(stat= "identity", #plot the given mean body mass values
           position= position_dodge()) + #creates clustered bar plot (by sex)
  geom_errorbar(aes(ymin= body_mass-sd, #creates the floor of the error bar
                      ymax= body_mass+sd), #creates the ceiling of the error bar
                position= position_dodge(0.9), #makes error bars in the middle of each bar
                width= 0.4) + #changes size of error bars
  labs(x= "Species", #x-axis label
       y= "Body Mass (g)", #y-axis label
       title= "Penguin Body Mass by Species and Sex", #plot title 
       fill= "Sex", #legend title
       caption= "Source: Palmer Station LTER / palmerpenguins package") + #adds caption of where we got the data
  scale_fill_manual(values= lacroix_palette("Pure", #use fun color palette based on La Croix to differentiate between sex
                                            type = "discrete")) + #tell color palette we are working with discrete data
  theme_bw() + #change theme so we have a white background with grey grid lines and black outline
  theme(axis.title= element_text(size= 11), #change axes text size
        plot.title= element_text(hjust= 0.5, #change title text size
                                 face= "bold"), #make title bold
        plot.caption.position = "plot") #make caption on the bottom right of the page

# Quick Celebration :) ---------------------------------------------------------
praise() #run to receive a well deserved compliment!

# Saving the Plot --------------------------------------------------------------
ggsave(here("Week_03", "Outputs", "Homework_penguin.png"), #saves the ggplot using an unbreakable file path
       width= 7, height= 5)  #saves plot at a specific length and width
