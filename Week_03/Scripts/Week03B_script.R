# MBIO 612: Week 3B, ggplot2
# Created by: Shelbie Ishimaru
# Created on: 20204-09-14
################################################################################
# Load Libraries ---------------------------------------------------------------
library(palmerpenguins)
library(tidyverse)
library(ggplot2)
library(here)
library(praise)
library(beyonce)
library(ggthemes)

# Read-in Data -----------------------------------------------------------------
glimpse(penguins) #gives us a column overview, shows first 10 lines of df

# Data Analysis: Plotting ------------------------------------------------------
#1.
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm)) + 
  geom_point() + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") #Review: simple plot with labelled x and y-axes

#2. 
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm)) + 
  geom_point() + 
  geom_smooth() + #add a best fit line, not super informative
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)")

#3.
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm)) + 
  geom_point() + 
  geom_smooth(method= "lm") + #best fit line= linear model
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)")

#4.
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm)) + 
  geom_point() + 
  geom_smooth(method= "lm", se= F) + #best fit line= linear model, REMOVES standard error bar
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)")

#5.
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species)) + #plot data by species 
  geom_point() + 
  geom_smooth(method= "lm") + #best fit line= linear model, regression line for each of the 3 different species
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)")

#6.
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + #change color of data points and regression line to reflect species type, NOTE: uses default colorway
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)")
 
#7.
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + #change color of data points and regression line to reflect species type
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_viridis_d() #changes the color scale 

# Quick Praise Break :) --------------------------------------------------------
praise() 

# Back to Plotting ------------------------------------------------------
#8.
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + 
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_viridis_d() +
  scale_x_continuous(limits= c(0,20)) #scale the x-axis from 0 to 20

#9.
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + 
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_viridis_d() +
  scale_x_continuous(limits= c(0,20)) +
  scale_y_continuous(limits= c(0,50)) #scale the y-axis from 0 to 50

#10.
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + 
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_viridis_d() +
  scale_x_continuous(breaks= c(14, 17, 21)) #label the x axis at 14, 17, and 21

#11.
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + 
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_viridis_d() +
  scale_x_continuous(breaks= c(14, 17, 21),
                     labels= c("Low", "Medium", "High")) #label the x axis at 14, 17, and 21 with words

#12.
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + 
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_manual(values= c("orange", "purple", "green")) #manually changes the color of points and regression lines by species

#13.
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + 
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_manual(values= beyonce_palette(18)) #manually changes the color of points and regression lines by species USING THE BEYONCE COLOR PALLETTE

#14.
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + 
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_manual(values= beyonce_palette(18)) +
  coord_flip() #flips the x and y axes

#15.
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + 
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_manual(values= beyonce_palette(18)) +
  coord_fixed() #fixes coordinates to a 1:1 ratio

#16. NOTE: Analysis below uses the diamond data set!
ggplot(diamonds, aes(carat, price)) + #simple plot: x= carat size, y= price (exponential increase)
  geom_point()

#17. 
ggplot(diamonds, aes(log(carat), log(price))) + #log the data, linearizes data but on a log scale (not good to interpret)
  geom_point()

#18. 
ggplot(diamonds, aes(carat, price)) + 
  geom_point() +
  coord_trans(x= "log10", y= "log10") #changes the coord system but keeps the data, axes have the natural value but scale is log!

#19. NOTE: back to penguin data
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + 
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_manual(values= beyonce_palette(18)) +
  coord_polar("x") #makes the x-axis in polar coords

#20. 
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + 
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_manual(values= beyonce_palette(18)) +
  theme_bw() #adds theme with white background with grey and black lines

#21. 
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + 
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_manual(values= beyonce_palette(18)) +
  theme_gray() #adds theme with grey background with white lines

#22. 
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + 
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_manual(values= beyonce_palette(18)) +
  theme_classic() #clean background with no background or line color

#23. 
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + 
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_manual(values= beyonce_palette(18)) +
  theme_wsj() #makes plot theme look like wall street journal

#20. 
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + 
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_manual(values= beyonce_palette(18)) +
  theme_bw() +
  theme(axis.title= element_text(size= 20)) #change the size within the theme

#21. 
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + 
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_manual(values= beyonce_palette(18)) +
  theme_bw() +
  theme(axis.title= element_text(size= 20, color= "red")) #change the size and axes label color within the theme

#22. 
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + 
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_manual(values= beyonce_palette(18)) +
  theme_bw() +
  theme(axis.title= element_text(size= 20, 
                                 color= "red"),
        panel.background = element_rect(fill= "linen")) #change the size, axes label color, and plot bakground within the theme

#23. 
ggplot(data= penguins, mapping= aes(x= bill_depth_mm, 
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + 
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_manual(values= beyonce_palette(18)) +
  theme_bw() +
  theme(axis.title= element_text(size= 20, 
                                 color= "red"),
        panel.background = element_rect(fill= "linen"),
        axis.ticks = element_line(color= "purple")) #changes the axes tick marks to purple

# Saving Plots -----------------------------------------------------------------
ggsave(here("Week_03", "Outputs", "Class_penguin.png"), width= 7, height= 5) #saves the ggplot to our specified folder and saves it at a specific length and width

plot1 <- ggplot(data= penguins, mapping= aes(x= bill_depth_mm, #makes the plot an object
                                    y= bill_length_mm,
                                    group= species,
                                    color= species)) + 
  geom_point() + 
  geom_smooth(method= "lm") + 
  labs(x= "Bill Depth (mm)", 
       y= "Bill Length (mm)") +
  scale_color_manual(values= beyonce_palette(18)) +
  theme_bw() +
  theme(axis.title= element_text(size= 20, 
                                 color= "red"),
        panel.background = element_rect(fill= "linen"),
        axis.ticks = element_line(color= "purple")) 

plot1 #allows you to see the plot

