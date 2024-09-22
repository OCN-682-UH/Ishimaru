# MBIO 612: Homework 4A
# Created by: Shelbie Ishimaru
# Created on: 2024-09-17
################################################################################
# Load Libraries ---------------------------------------------------------------
library(palmerpenguins) #so we can load in our penguin data
library(tidyverse) #for data manipulation
library(here) #used to create unbreakable file paths
library(beyonce) #a fun color palette
library(dadjoke) #for motivation/celebration!

# Read-in Data -----------------------------------------------------------------
glimpse(penguins) #gives us a column overview, shows first 10 lines of df

# Data Analysis: Question 1 ----------------------------------------------------
#Goal: calculate the mean and variance of body mass by species, island, and sex without any NAs
question1 <- penguins %>% #creates a new df and pulls in our penguin data set
  na.omit() %>% #removes all NAs
  group_by(species, island, sex) %>% #groups by our desired variables species, sex, and island
  summarize(mean_mass= mean(body_mass_g, na.rm= T), #calculates the mean body mass
            mass_var= var(body_mass_g, na.rm= T)) #calculates variance of body mass

# Data Analysis: Question 2 ----------------------------------------------------
(question2 <- penguins %>% #creates a new df and pulls in our penguin data set
  filter(sex== "female") %>% #removes all male instances
  mutate(log_mass= log(body_mass_g)) %>% #creates a new df column and calculates the log body mass
  select(species, island, sex, log_mass) %>% #selects the columns we want in our final df
  ggplot(aes(x= species, #x-axis is the different penguin species by island
             y= log_mass,  #y-axis is the calculated log of body mass in g
             fill= island)) + #color each box by island
  geom_boxplot() + #create a box plot given the log body mass values
  labs(x= "Species", #x-axis label
       y= "log(Body Mass) (g)", #y-axis label
       title= "Penguin Body Mass by Species and Island", #plot title 
       fill= "Island", #legend title
       caption= "Source: Palmer Station LTER / palmerpenguins package") + #adds caption of where we got the data
  scale_fill_manual(values= beyonce_palette(101)) + #uses the beyonce color palette to color each box by island
  theme_bw() + #change theme so we have a white background with grey grid lines and black outline
  theme(axis.title= element_text(size= 11), #change axes text size
        plot.title= element_text(hjust= 0.5, #change title text size
                                 face= "bold"), #make title bold
        plot.caption.position = "plot")) %>% #make caption on the bottom right of the page
  ggsave(here("Week_04", "Outputs", "Homework_plotA.png"), #saves the final plot using an unbreakable file path
         ., width= 9, height= 5) #uses dot notation as a place holder to allow plotting, then gives desired plot dimensions

# Quick Celebration :) ---------------------------------------------------------
dadjoke() #run to receive a dad joke!
