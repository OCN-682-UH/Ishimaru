Tidy Tuesday 5
================
Shelbie Ishimaru
2024-11-22

#### What I learned this week!

This week I learned how to create a animated line plot. When making
animated plots I usually use transition_states() within gganimate
package, however this time I used transition_reveal(). This different
transition type causes the lines to appear over the years and achieve
the animated line plot. I also learned how to slow down this animation
using animate(). Along with this new plot, I used the fishualize
package, which are fun R color palettes that are based on fish! I chose
to use the *Scarus quoyi* color palette and attached an image of the
inspiration.
![title](https://www.fishi-pedia.com/wp-content/uploads/2024/04/Scarus-quoyi-Scaridae-20170531-Anda_PH-FLFISHI-scaled.jpg)

#### Load Libraries

``` r
library(tidyverse) #for data manipulation
library(lubridate) #for working with dates
library(gganimate) #to create an animated plot
library(fishualize) #a fun fish inspired color palette 
```

#### Read-in Data

``` r
cbp_state <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-26/cbp_state.csv') #read in US customs and boarder protection encounter data 
```

#### Data Manipulation

``` r
cbp_data <- cbp_state %>% #pull in data set
  filter(land_border_region== "Northern Land Border") %>% #only keep our boarder of interest
  mutate(month_abbv= factor(month_abbv, levels= c("JAN", #factor and order the months
                                      "FEB",
                                      "MAR",
                                      "APR",
                                      "MAY",
                                      "JUN",
                                      "JUL",
                                      "AUG",
                                      "SEP",
                                      "OCT",
                                      "NOV",
                                      "DEC"), ordered= T)) %>%
  mutate(state= factor(state, levels= c("WA", #factor and order states so they display from east to west in the legend 
                                        "MT",
                                        "ND",
                                        "MN",
                                        "MI",
                                        "NY",
                                        "ME"), ordered= T)) %>%
  group_by(fiscal_year, month_abbv, state) %>% #select columns for analysis
  summarise(total_encounters= sum(encounter_count)) %>% #sum encoutners by year, month, and state
  filter(state!= "PA" & state!= "AK" & state!= "OH" & state!= "MN" & state!= "ID") %>% #remove states that don't have data every month
  mutate(month_abbv= recode(month_abbv, #change month abbreviation from characters to numeric (to support date creation)
                            "JAN"= 1,
                            "FEB"= 2,
                            "MAR"= 3,
                            "APR"= 4,
                            "MAY"= 5,
                            "JUN"= 6,
                            "JUL"= 7,
                            "AUG"= 8,
                            "SEP"= 9,
                            "OCT"= 10,
                            "NOV"= 11,
                            "DEC"= 12)) %>%
  mutate(dates= my(paste(month_abbv, fiscal_year, sep="-"))) #make a column that joins month and year and make it a true date value (for animated plotting)
```

#### Create Plot

``` r
cbp_plot <- ggplot(cbp_data, aes(x= dates, y= total_encounters, color= state)) + #initialize plot
  geom_line() + #make it a line plot
  geom_point() + #add points for animation
  transition_reveal(dates) + #animate by dates column 
  scale_color_fish_d(option= "Scarus_quoyi") + #color using fishualize!
  labs(title= "Total US Northern Land Border Encounters by State from 2020 to 2024", #add plot title
       x= "Year", #add x-axis title
       y= "Total Encounters", #add y-axis title
       color= "State", #add legend title
       caption= "Source: Data Science Learning Community. 2024. Tidy Tuesday: A weekly social data project.") + #add caption to credit data source
  theme_bw() + #nice simple theme
  theme(plot.title= element_text(hjust= 0.5, face= "bold", size= 28), #change text size and bold title
        axis.text= element_text(size= 13), #change axis values size
        axis.title= element_text(size= 16), #change axis title size
        plot.caption= element_text(size= 10), #change caption text size and bold title
        plot.caption.position = "plot", #make the caption in the bottom right corner
        legend.title= element_text(size= 16), #change legend title size
        legend.text= element_text(size= 13)) #change legend text size
animate(cbp_plot, nframes= 500) #slow animation speed
```

![](../Output/final_plot-1.gif)<!-- -->
