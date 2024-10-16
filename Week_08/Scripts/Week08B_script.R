# MBIO 612: Week 8A
# Created by: Shelbie Ishimaru
# Created on: 2024-10-15
################################################################################
# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(here)
library(patchwork)
library(ggrepel)
library(gganimate)
library(magick)
library(palmerpenguins)

# Patchwork --------------------------------------------------------------------
#Easily brings plots together
#plot 1
p1<-penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_length_mm, 
             color = species))+
  geom_point()
p1

#plot 2
p2<-penguins %>%
  ggplot(aes(x = sex, 
             y = body_mass_g, 
             color = species))+
  geom_jitter(width = 0.2)
p2

p1+p2 + #pastes the 2 plots together next to eachother
  plot_layout(guides = 'collect') + #group legends
  plot_annotation(tag_levels = 'A') #add labels

p1/p2 + #pastes the 2 plots together on top of eachother
  plot_layout(guides = 'collect')+
  plot_annotation(tag_levels = 'A')

# ggrepel ----------------------------------------------------------------------
#easy and clear labels for plots
View(mtcars) #look at mtcars data

ggplot(mtcars, aes(x = wt, 
                   y = mpg, 
                   label = rownames(mtcars))) +
  geom_text() + #creates a text label
  geom_point(color = 'red')


ggplot(mtcars, aes(x = wt, 
                   y = mpg, 
                   label = rownames(mtcars))) +
  geom_text_repel() + #repel them
  geom_point(color = 'red')


ggplot(mtcars, aes(x = wt, 
                   y = mpg, 
                   label = rownames(mtcars))) +
  geom_label_repel() + #Label repel them
  geom_point(color = 'red')

# gganimate --------------------------------------------------------------------
#make your figure an animation
penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  transition_states(
    year, # what are we animating by
    transition_length = 2, #The relative length of the transition.
    state_length = 1) + # The length of the pause between transitions
  ease_aes("bounce-in-out") + #types= linear (default), bounce, sine, circular...
  labs(title = 'Year: {closest_state}') 
  anim_save(here("Week_08","Outputs","mypengiungif.gif"))

# magick -----------------------------------------------------------------------
#advanced image processing
penguin<-image_read("https://pngimg.com/uploads/penguin/pinguin_PNG9.png")
penguin #save panguin image

penguins %>% #make plot
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point()
ggsave(here("Week_08","Outputs","penguinplot.png")) #save plot

penplot<-image_read(here("Week_08","Outputs","penguinplot.png")) #add penguin image to plot, always read-in the background first!
out <- image_composite(penplot, penguin, offset = "+70+30") #where to put the image on the plot
out

# Read in a penguin gif
pengif<-image_read("https://media3.giphy.com/media/H4uE6w9G1uK4M/giphy.gif")
outgif <- image_composite(penplot, pengif, gravity = "center")
animation <- image_animate(outgif, fps = 10, optimize = TRUE)
animation

# Fun R Package! ---------------------------------------------------------------
library(sourrr)
build_recipe(final_weight = 900, hydration = 0.75)
