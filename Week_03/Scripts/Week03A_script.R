# MBIO 612: Week 3A, Learning ggplot2
# Created by: Shelbie Ishimaru
# Created on: 2024-09-10
################################################################################
# Load Libraries ---------------------------------------------------------------
library(palmerpenguins)
library(tidyverse)
library(ggplot2)
# Read-in Data -----------------------------------------------------------------
View(penguins) #look at the entire data set 
head(penguins) #gives us the top 6 rows
tail(penguins) #gives us the last 6 rows
glimpse(penguins) #gives us a column overview 

# Initial Plotting -------------------------------------------------------------
#Coded step by step below:
#1.
ggplot(data= penguins) #creates an empty plot

#2.
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm)) #creates the x-axis

#3.
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm)) #creates the y-axis

#4.
ggplot(data= penguins,
      mapping= aes(x= bill_depth_mm,
                   y= bill_length_mm)) +
  geom_point() #adds points to the plot

#5.
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm, 
                    color= species)) +
  geom_point() #colors points by species and creates baseline legend

#6.
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm, 
                    color= sex)) +
  geom_point() #colors points by sex and creates baseline legend

#7.
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm, 
                    color= island)) +
  geom_point() #colors points by island and creates baseline legend

#8.
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm, 
                    color= species)) +
  geom_point() +
  labs(title= "Bill depth and length") #Adds title

#9.
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm, 
                    color= species)) +
  geom_point() +
  labs(title= "Bill depth and length",
       subtitle= "Dimensions for Adelie, Chinstrap, and Gentoo Penguins") #adds subtitle

#10. 
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm, 
                    color= species)) +
  geom_point() +
  labs(title= "Bill depth and length",
       subtitle= "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x= "Bill Depth (mm)", y= "Bill Length (mm)") #creates more informative axis labels


#11.
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm, 
                    color= species)) +
  geom_point() +
  labs(title= "Bill depth and length",
       subtitle= "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x= "Bill Depth (mm)", y= "Bill Length (mm)",
       color= "Species") #changes legend title


#12. 
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm, 
                    color= species)) +
  geom_point() +
  labs(title= "Bill depth and length",
       subtitle= "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x= "Bill Depth (mm)", y= "Bill Length (mm)",
       color= "Species",
       caption= "Source: Palmer Station LTER / palmerpenguins package") #adds caption/data source

#13. 
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm, 
                    color= species)) +
  geom_point() +
  labs(title= "Bill depth and length",
       subtitle= "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x= "Bill Depth (mm)", y= "Bill Length (mm)",
       color= "Species",
       caption= "Source: Palmer Station LTER / palmerpenguins package") +
  scale_color_viridis_d() #changes colors to account for color blindness (NOTE: NOT THE BEST OPTION w/ yellow on grey)

# Aesthetic Plotting -----------------------------------------------------------
#1. 
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm, 
                    color= species,
                    shape= island)) +
  geom_point() +
  labs(title= "Bill depth and length",
       subtitle= "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x= "Bill Depth (mm)", y= "Bill Length (mm)",
       color= "Species",
       caption= "Source: Palmer Station LTER / palmerpenguins package") +
  scale_color_viridis_d() #changes point shape by island and creates 2nd legend

#2.
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm, 
                    color= species,
                    shape= species)) +
  geom_point() +
  labs(title= "Bill depth and length",
       subtitle= "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x= "Bill Depth (mm)", y= "Bill Length (mm)",
       color= "Species",
       caption= "Source: Palmer Station LTER / palmerpenguins package") +
  scale_color_viridis_d() #both color and shape of points to be based off of species 

#3. 
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm, 
                    color= species,
                    size= body_mass_g)) +
  geom_point() +
  labs(title= "Bill depth and length",
       subtitle= "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x= "Bill Depth (mm)", y= "Bill Length (mm)",
       shape= "Species",
       caption= "Source: Palmer Station LTER / palmerpenguins package") +
  scale_color_viridis_d() #changes point size based on penguin body mass

#4. 
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm, 
                    color= species,
                    size= body_mass_g,
                    alpha= flipper_length_mm)) +
  geom_point() +
  labs(title= "Bill depth and length",
       subtitle= "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x= "Bill Depth (mm)", y= "Bill Length (mm)",
       shape= "Species",
       caption= "Source: Palmer Station LTER / palmerpenguins package") +
  scale_color_viridis_d() #changes point color based on flipper length (smaller flipper= more transparent and the converse)

#Faceting ----------------------------------------------------------------------
#1. 
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm)) +
  geom_point() +
  facet_grid(species~sex) #Faceting: every row is species and col is sex, includes NAs for sex

#2.
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm)) +
  geom_point() +
  facet_grid(sex~species) #Faceting: every row is sex and col is species

#3.
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm)) +
  geom_point() +
  facet_wrap(~species) #facet wrap: allows you to determine the dims/control whats happening (rows by colums, the ~ matters)

#4. 
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm)) +
  geom_point() +
  facet_wrap(~species, ncol= 2) #forces two columns

#5.
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm, 
                    color= species,
                    shape= island)) +
  geom_point() +
  labs(title= "Bill depth and length",
       subtitle= "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x= "Bill Depth (mm)", y= "Bill Length (mm)",
       color= "Species",
       caption= "Source: Palmer Station LTER / palmerpenguins package") +
  scale_color_viridis_d()+
  facet_grid(species~sex) #adds faceting to colored plots (plot with AES)

#6. 
ggplot(data= penguins,
       mapping= aes(x= bill_depth_mm, 
                    y= bill_length_mm, 
                    color= species,
                    shape= island)) +
  geom_point() +
  labs(title= "Bill depth and length",
       subtitle= "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x= "Bill Depth (mm)", y= "Bill Length (mm)",
       color= "Species",
       caption= "Source: Palmer Station LTER / palmerpenguins package") +
  scale_color_viridis_d()+
  facet_grid(species~sex)+
  guides(color= F) #deletes the color legend
