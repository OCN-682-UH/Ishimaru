# MBIO 612: Homework 4A
# Created by: Shelbie Ishimaru
# Created on: 2024-09-17
################################################################################
# Load Libraries ---------------------------------------------------------------
library(palmerpenguins) #so we can load in our penguin data
library(tidyverse) #for data manipulation
library(here) #used to create unbreakable file paths
library(dadjoke) #for motivation/celebration!

# Read-in Data -----------------------------------------------------------------
glimpse(penguins) #gives us a column overview, shows first 10 lines of df

# Data Analysis: Question 1 ----------------------------------------------------
question1 <- penguins %>%
  drop_na(sex) %>% 
  na.omit() %>%
  group_by(species, island, sex) %>%
  summarize(mean_mass= mean(body_mass_g, na.rm= T), 
            mass_var= var(body_mass_g, na.rm= T))

# Data Analysis: Question 2 ----------------------------------------------------
question2 <- penguins %>% 
  filter(sex== "female") %>%
  mutate(log_mass= log(body_mass_g)) %>%
  select(species, island, sex, log_mass) 
#%>% ggplot(aes(x= sex, y= flipper_length_mm)) + geom_boxplot()
# ggsave(here("Week_03", "Outputs", "Homework_penguin.png"), #saves the ggplot using an unbreakable file path 
#width= 7, height= 5)  #saves plot at a specific length and width

# Quick Celebration :) ---------------------------------------------------------
dadjoke() #run to receive a dad joke!