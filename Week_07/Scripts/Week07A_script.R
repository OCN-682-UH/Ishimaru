# MBIO 612: Week 7A
# Created by: Shelbie Ishimaru
# Created on: 2024-10-08
################################################################################
# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(here)
library(maps)
library(mapdata)
library(mapproj)

# Read in data on population in California by county ---------------------------
popdata<-read_csv(here("Week_07","data","CApopdata.csv"))
glimpse(popdata) #look at data

# Read in data on number of sea stars at different field sites -----------------
stars<-read_csv(here("Week_07","data","stars.csv"))
glimpse(stars) #look at data

# Get data for the entire world ------------------------------------------------
world<-map_data("world") #gives huge df
head(world) #shows the first 6 rows of the huge world df

# Get data for the USA ---------------------------------------------------------
usa<-map_data("usa")
head(usa)

# Get data for italy -----------------------------------------------------------
italy<-map_data("italy")
head(italy)

# Get data for continental US states -------------------------------------------
states<-map_data("state")
head(states)

# Get data for continental US counties -----------------------------------------
counties<-map_data("county")
head(counties)

# Make a map of the world ------------------------------------------------------
ggplot() + #very simple and kinda ugly
  geom_polygon(data = world, #create a world map
               aes(x = long, y = lat, group = group, #group= group is VERY important!
                   fill = region), #fill countries with different colors
               color = "black") + #outline the countries in black 
  guides(fill = FALSE) + #remove the legend
  theme_minimal() + #clean background
  theme(panel.background = element_rect(fill = "lightblue")) + #make the background blue like the ocean
  coord_map(projection = "mercator", #change projection 
            xlim = c(-180,180))

# Make a map of California -----------------------------------------------------
head(states) #view the dataset

CA_data<-states %>%
  filter(region == "california") #filter out CA data
head(CA_data) #view new CA df

ggplot() + #very simple and kinda ugly
  geom_polygon(data = CA_data, #create a world map
               aes(x = long, y = lat, group = group)) + #group= group is VERY important!
  theme_minimal() + #clean background
  theme(panel.background = element_rect(fill = "lightblue")) + #make the background blue like the ocean
  coord_map(projection = "mercator")
#use theme_void() to get just the shape!

# Color each county by population density --------------------------------------
# Look at the county data
head(counties)[1:3,] # only showing the first 3 rows for space
head(popdata) # Look at the county data

#join the two dfs, look at the janitor pkg for data cleaning
CApop_county<-popdata %>%
  select("subregion" = County, Population)  %>% #rename the county col
  inner_join(counties) %>% #inner join bc we don't want any NAs
  filter(region == "california") #some counties have same names in other states
head(CApop_county) #Look at the county data

#Make map of California and fill counties by population density
ggplot()+
  geom_polygon(data = CApop_county, 
               aes(x = long, 
                   y = lat, 
                   group = group,
                   fill = Population),
               color = "black")+
  coord_map()+
  theme_void() +
  scale_fill_gradient(trans = "log10") #log the population density so that the outlier of LA does not skew our map

# Relate the population density of humans to the population density of sea stars
head(stars) #look at sea stars

ggplot()+
  geom_polygon(data = CApop_county, 
               aes(x = long, 
                   y = lat, 
                   group = group,
                   fill = Population),  
               color = "black")+
  geom_point(data = stars, # add a point at all my sites for sea star obs sites
             aes(x = long,
                 y = lat,
                 size = star_no))+ #relate the number of sea stars to the size of the dot
  coord_map()+
  theme_void() +
  scale_fill_gradient(trans = "log10") +
  labs(size = "# stars/m2") #give star legend an informative title

ggsave(here("Week_07","output","CApop.pdf")) #save plot!

#Fun r-package of the day
remotes::install_github("R-CoderDotCom/ggdogs@main")
library(ggdogs)
ggplot(mtcars) +
  geom_dog(aes(mpg, wt), dog = "pug", size = 5)

