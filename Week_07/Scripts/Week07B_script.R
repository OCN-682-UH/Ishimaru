# MBIO 612: Week 7A
# Created by: Shelbie Ishimaru
# Created on: 2024-10-14
################################################################################
# Load libraries ---------------------------------------------------------------
library(ggmap)
library(tidyverse)
library(here)
library(ggspatial)
library(emojifont)

# Read in data -----------------------------------------------------------------
ChemData<-read_csv(here("Week_07","data","chemicaldata_maunalua.csv"))
glimpse(ChemData)

# Basemaps ---------------------------------------------------------------------
Oahu<-get_map("Oahu") #location as a string
ggmap(Oahu) #using ggmap to plot base layer

#Make a data frame of lon and lat coordinates
WP<-data.frame(lon = -157.7621, lat = 21.27427) # coordinates for Wailupe
Map1<-get_map(WP) #get base layer
ggmap(Map1) #plot it

Map1<-get_map(WP,zoom = 17) #zoom in on a location (ranges from 3 (far out) to 20 (very specific))
ggmap(Map1)

Map1<-get_map(WP,zoom = 17, maptype = "satellite") #change map type
ggmap(Map1)

Map1<-get_map(WP,zoom = 17, maptype = "stamen_watercolor", source = "stadia") #change map type again
ggmap(Map1)

# Using ggmap with data --------------------------------------------------------
Map1<-get_map(WP,zoom = 17, maptype = "satellite") 
ggmap(Map1)+
  geom_point(data = ChemData,
             aes(x = Long, y = Lat, color = Salinity),
             size = 4) +
  scale_color_viridis_c() +
  annotation_scale( bar_cols = c("yellow", "white"),
                    location = "bl")+ # put the bar on the bottom left and make the colors yellow and white
  annotation_north_arrow(location = "tl")+ # add a north arrow
  coord_sf(crs = 4326) # for the scale bar to work it needs to be in this coordinate system - this is a typical coordinate reference system for a GPS (WGS84)

# Find latlongs ----------------------------------------------------------------
geocode("the white house")
geocode("University of Hawaii at Manoa")

# Fun R package ----------------------------------------------------------------
ggplot() + 
  geom_emoji('smile_cat', 
             x=1:5, y=1:5, size=10)
