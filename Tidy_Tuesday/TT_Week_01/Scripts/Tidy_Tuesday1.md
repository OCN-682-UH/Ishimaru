Tidy Tuesday 1
================
Shelbie Ishimaru
2024-11-02

#### What I learned this week!

This week I expanded my knowledge of maps by learning about the
rnaturalearth package. Along with this new type of map data, I also
wanted to continue developing my animation experience and pushed myself
to create a map that showed change of a variable over time. From this
assignment I found that creating a animated map was a lot harder than
animating a lollipop plot. Although it was tricky, it was very rewarding
once I found the right stackoverflow forum to help me debug and I
produced a final product!

#### Load Libraries

``` r
library(rnaturalearth) #for creating maps
library(rnaturalearthdata) #for creating maps 
library(transformr) #for creating map with the sf package
library(tidyverse) #for data manipulation
library(gganimate) #for animating the map
```

#### Read-in Data

``` r
democracy_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-05/democracy_data.csv') #read in data from tidy tuesday 
#glimpse(democracy_data) #look at data, commented out because its a large df

world <- ne_countries(scale = "medium", returnclass = "sf") #read in map data to use sf package
#head(world) #look at data, commented out because its a large df
```

#### Data Manipulation

``` r
democracy_data <- democracy_data %>% #call tidy tuesday data
  select(country_name, year, is_monarchy) %>% #save only the columns needed for analysis
  rename(admin= country_name) %>% #rename the column for later join
  mutate(admin= str_replace(admin, "United States", "United States of America")) %>% #edit country name for later join (so it we see values in the final map!)
  mutate(admin= str_replace(admin, "Congo, Dem. Rep.", "Democratic Republic of the Congo")) %>% #edit country name for later join (so it we see values in the final map!)
  mutate(admin= str_replace(admin, "Congo, Republic of", "Republic of the Congo")) %>% #edit country name for later join (so it we see values in the final map!)
  mutate(admin= str_replace(admin, "Tanzania", "United Republic of Tanzania")) %>% #edit country name for later join (so it we see values in the final map!)
  mutate(admin= str_replace(admin, "Côte d`Ivoire", "Ivory Coast")) #edit country name for later join (so it we see values in the final map!)
  
world_monarch <- world %>% #call sf map data
  select(admin, geometry) %>% #save only columns needed for analysis
  inner_join(democracy_data) %>% #join the tidy tuesday and sf data using admin (country name)
  filter(year < 2020) %>% #remove 2020 data becuase it was creating odd output
  mutate(is_monarchy= str_replace(is_monarchy, "TRUE", "Yes")) %>% #edit wording for final map legend
  mutate(is_monarchy= str_replace(is_monarchy, "FALSE", "No")) #edit wording for final map legend
glimpse(world_monarch) #look at finalized data
```

    ## Rows: 13,020
    ## Columns: 4
    ## $ admin       <chr> "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe", "Zimbabwe"…
    ## $ year        <dbl> 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958, 1959…
    ## $ is_monarchy <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No", "No"…
    ## $ geometry    <MULTIPOLYGON [°]> MULTIPOLYGON (((31.28789 -2..., MULTIPOLYGON …

#### Create Plot

``` r
map_cols <- c("deepskyblue2", "indianred3", "grey55") #create color scheme for map

ggplot() + #initiate plot
  geom_sf(data= world, size = .1) + #create base map of the world
  geom_sf(aes(fill= is_monarchy), data= world_monarch) + #fill in countires by monarchy status
  transition_states(as.numeric(year)) + #animate by year
  labs(title= "Presence of Monarchies Around the World from 1950-2019", #add map title
       subtitle= "Year= {closest_state}", #add subtitle that informs viewers of the current year
       fill= "Monarchy Present", #add legend title
       caption= 'Source: Bjørnskov and Rode. 2020.') + #add caption to provide citation
  scale_fill_manual(values= map_cols) + #utilize color scheme
  theme_bw() + #change theme so we have a white background with grey grid lines and black outline
    theme(plot.title= element_text(hjust= 0.5, face= "bold", size= 14), #change text size, bold text, and center title
        plot.subtitle= element_text(size= 9), #change text size of subtitle
        plot.caption= element_text(size= 7), #change text size of caption
        plot.caption.position = "plot") #make caption on the bottom right of the page
```

![](../Output/Monarch_Map-1.gif)<!-- -->
