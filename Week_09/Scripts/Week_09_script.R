# MBIO 612: Week 9A
# Created by: Shelbie Ishimaru
# Created on: 2024-10-22
################################################################################
# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(palmerpenguins)
library(PNWColors)

# Create Fake df ---------------------------------------------------------------
df <- tibble(
  a = rnorm(10), #draws 10 random values from a normal distribution
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
head(df) #look at df

# EX: Rescale df ---------------------------------------------------------------
#Manually doing it
df<-df %>%
  mutate(a = (a-min(a, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(a, na.rm = TRUE))) #rescale one column individually

df<-df %>%
  mutate(a = (a-min(a, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(a, na.rm = TRUE)), #rescale every column 
         b = (b-min(b, na.rm = TRUE))/(max(b, na.rm = TRUE)-min(b, na.rm = TRUE)),
         c = (c-min(c, na.rm = TRUE))/(max(c, na.rm = TRUE)-min(c, na.rm = TRUE)),
         d = (d-min(d, na.rm = TRUE))/(max(d, na.rm = TRUE)-min(d, na.rm = TRUE)))

#Creating a function to do it 
rescale01 <- function(x) { #create rescale function
  value<-(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
  return(value)
}
df %>%
  mutate(a = rescale01(a), #utilize newly created function
         b = rescale01(b),
         c = rescale01(c),
         d = rescale01(d))

# EX: Temperature Conversion ---------------------------------------------------
temp_C <- (temp_F - 32) * 5 / 9 #F to C calculation 

#Write a function
fahrenheit_to_celsius <- function(temp_F) { 
  temp_C <- (temp_F - 32) * 5 / 9 
  return(temp_C)
}
fahrenheit_to_celsius(32) #test function
fahrenheit_to_celsius(212)

# Think, Pair, Share -----------------------------------------------------------
#Write a function that converts Celsius to kelvin. (Remember Kelvin is Celsius + 273.15)
celsius_to_kelvin <- function(temp_C) { 
  temp_K <- temp_C + 273.15
  return(temp_K)
}
celsius_to_kelvin(100)

# Functions and Plotting -------------------------------------------------------
#Regular plotting
pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm, color = island))+
  geom_point()+
  geom_smooth(method = "lm")+ # add a linear model
  scale_color_manual("Island", values=pal)+   # use pretty colors and another example of how to manually change the legend title for colors
  theme_bw()

#Write a plotting function
myplot<-function(data = penguins, x, y){
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
    geom_point()+
    geom_smooth(method = "lm")+ # add a linear model
    scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
    theme_bw()
}
myplot(data = penguins, x = body_mass_g, y = bill_length_mm) #test plotting function
myplot(data = penguins, x = body_mass_g, y = flipper_length_mm) #testing function with different vars
myplot(x = body_mass_g, y = flipper_length_mm) #don't need to specify the data set

myplot(x = body_mass_g, y = flipper_length_mm)+ #layering the plot
  labs(x = "Body mass (g)",
       y = "Flipper length (mm)")

# Adding Ifelse statements -----------------------------------------------------
#Example
a <- 4
b <- 5

if (a > b) { # my question
  f <- 20 # if it is true give me answer 1
} else { # else give me answer 2
  f <- 10
}
f

#Back to plotting
myplot<-function(data = penguins, x, y, lines=TRUE ){ # add new argument for lines
  pal<-pnw_palette("Lake",3, type = "discrete") # my color palette 
  if(lines==TRUE){
    ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
      geom_point()+
      geom_smooth(method = "lm")+ # add a linear model
      scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
      theme_bw()
  }
  else{
    ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
      geom_point()+
      scale_color_manual("Island", values=pal)+   # use pretty colors and change the legend title
      theme_bw()
  }
}
myplot(x = body_mass_g, y = flipper_length_mm) #test it with lines
myplot(x = body_mass_g, y = flipper_length_mm, lines = FALSE) #test it without lines
