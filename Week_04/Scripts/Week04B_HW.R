# MBIO 612: Homework 4B
# Created by: Shelbie Ishimaru
# Created on: 2024-09-22
################################################################################
# Load Libraries ---------------------------------------------------------------
library(tidyverse) #for data manipulation
library(here) #used to create unbreakable file paths
library(LaCroixColoR) #a fun color palette!
library(praise) #for motivation/celebration!

# Read-in Data -----------------------------------------------------------------
ChemData<-read_csv(here("Week_04","Data", "chemicaldata_maunalua.csv")) #read in new data set
View(ChemData) #look at entire df in new window

# Data Analysis ----------------------------------------------------------------
(ChemData_clean<-ChemData %>% #creates a new df and pulls in our biogeochem data set
  drop_na() %>% #filters out everything that is not a complete row
  separate(col = Tide_time, #choose the tide time col
           into = c("Tide","Time"), #separate it into two columns tide and time
           sep = "_") %>% #separate by _
  select(Site, Season, Tide, Time, Temp_in, Silicate, pH) %>% #selects the cols we are interested for calcualtions
  pivot_longer(cols = Temp_in:pH, #the cols you want to pivot. This says select the temp, silicate, and pH cols
               names_to = "Variables", #the names of the new cols with all the column names
               values_to = "Values") %>% #names of the new column with all the values
  group_by(Variables, Season, Tide, Time) %>% #groups by our desired variables
  summarise(mean_vals = mean(Values, na.rm = T)) %>% #calulates the mean temp, silicate, and pH by tide, season, and time
  write_csv(here("Week_04","Outputs","HW4B_data.csv")) %>% #exports pivoted df with calculations as a csv using an unbreakable file path
  filter(Variables=="Silicate") %>% #filters out temp and pH, becuase we are itnerested in silicate for plotting
  ggplot(aes(x = Variables, #x-axis is silicate
             y = mean_vals, #y-axis is silicate concentration
             fill= Time))+  #color the bars by time
  geom_bar(stat= "identity", #plot the given mean silicate concentrations
           position= position_dodge()) + #creates clustered bar plot (by time) 
  facet_grid(Season~Tide) + #create different plots for each season and tide
  labs(x= "", #x-axis label
       y= "Silicate Concentration (umol/L)", #y-axis label
       title= "Changes in Silicate by Time, Season, and Tide", #plot title 
       fill= "Time of Day", #legend title
       caption= "Source: Silbiger et al. 2020") + #adds caption of where we got the data
  scale_fill_manual(values= lacroix_palette("CranRaspberry", #use fun color palette based on La Croix to differentiate between time
                                            type = "discrete")) +#tell color palette we are working with discrete data
  theme_bw() + #change theme so we have a white background with grey grid lines and black outline
  theme(axis.title= element_text(size= 11), #change axes text size
        plot.title= element_text(hjust= 0.5, #change title text size
                                 face= "bold"), #make title bold
        plot.caption.position = "plot")) + #make caption on the bottom right of the page
  ggsave(here("Week_04", "Outputs", "Homework_plotB.png"), #saves the ggplot using an unbreakable file path
         width= 7, height= 5) #gives desired plot dimensions
#NOTE: I get a error with ggsave() but it still saves the final plot to the correct file path

# Quick Celebration :) ---------------------------------------------------------
praise() #run to receive a well deserved compliment!
