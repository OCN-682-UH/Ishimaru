# MBIO 612: Week 4B
# Created by: Shelbie Ishimaru
# Created on: 2024-09-20
################################################################################
# Load Libraries ---------------------------------------------------------------
library(tidyverse) #for data manipulation
library(here) #used to create unbreakable file paths
library(ggbernie) #change points to bernie

# Read-in Data -----------------------------------------------------------------
ChemData<-read_csv(here("Week_04","Data", "chemicaldata_maunalua.csv")) #read in new data set
View(ChemData) #look at entire df in new window
glimpse(ChemData) #look at the first several rows

# Clean Data -------------------------------------------------------------------
ChemData_clean<-ChemData %>% #call data
  filter(complete.cases(.)) #filters out everything that is not a complete row
View(ChemData_clean) #look at entire new df

#Separate
ChemData_clean<-ChemData %>%
  drop_na() %>% #filters out everything that is not a complete row
  separate(col = Tide_time, #choose the tide time col
           into = c("Tide","Time"), #separate it into two columns tide and time
           sep = "_", #separate by _
           remove = F) %>% #keep the original tide_time column
head(ChemData_clean) #look at updated df

#Unite
ChemData_clean<-ChemData %>%
  drop_na() %>% #filters out everything that is not a complete row
  separate(col = Tide_time, #choose the tide time col
           into = c("Tide","Time"), #separate it into two columns Tide and time
           sep = "_", #separate by _
           remove = F) %>% #keep the original tide_time column
  unite(col = "Site_Zone", #the name of the NEW col
        c(Site,Zone), #the columns to unite
        sep = ".", #lets put a . in the middle
        remove = F) #keep the original
head(ChemData_clean) #look at updated df

#Pivot longer: going from wide to long data
ChemData_long <-ChemData_clean %>%
  pivot_longer(cols = Temp_in:percent_sgd, #the cols you want to pivot. This says select the temp to percent SGD cols
               names_to = "Variables", #the names of the new cols with all the column names
               values_to = "Values") #names of the new column with all the values
View(ChemData_long) #look at pivoted df

# Data Analysis with Long Data -------------------------------------------------
ChemData_long %>%
  group_by(Variables, Site) %>% #group by everything we want
  summarise(Param_means = mean(Values, na.rm = T), #get mean
            Param_vars = var(Values, na.rm = T)) #get variance

#PRACTICE: calculate mean, variance, and sd by site, zone, and tide
ChemData_long %>%
  group_by(Variables, Zone, Site, Tide) %>% #group by everything we want 
  summarise(Param_means = mean(Values, na.rm = T), #get mean 
            Param_vars = var(Values, na.rm = T), #get variance
            Param_sd = sd(Values, na.rm = T)) #get standard deviation

#Facet wrap with long data
#1. 
ChemData_long %>%
  ggplot(aes(x = Site, y = Values))+
  geom_boxplot()+
  facet_wrap(~Variables) #creates box plot of each variable by site

#2. 
ChemData_long %>%
  ggplot(aes(x = Site, y = Values))+ 
  geom_boxplot()+ 
  facet_wrap(~Variables, scales = "free") #changes the axes scales so the plots are more informative, "free_y" only frees y-axis

#Pivot wider: going from long to wide df
ChemData_wide<-ChemData_long %>%
  pivot_wider(names_from = Variables, #column with the names for the new columns
              values_from = Values) #column with the values
View(ChemData_wide)

# Restart: from beginning to end -----------------------------------------------
#View() data in console every line!
#1.
ChemData_clean<-ChemData %>%
  drop_na()  #filters out everything that is not a complete row

#2.
ChemData_clean<-ChemData %>%
  drop_na() %>% #filters out everything that is not a complete row
  separate(col = Tide_time, #choose the tide time col
           into = c("Tide","Time"), #separate it into two columns Tide and time
           sep = "_", #separate by _
           remove = F)

#3.
ChemData_clean<-ChemData %>%
  drop_na() %>% #filters out everything that is not a complete row
  separate(col = Tide_time, #choose the tide time col
           into = c("Tide","Time"), #separate it into two columns Tide and time
           sep = "_", #separate by _
           remove = F) %>%
  pivot_longer(cols = Temp_in:percent_sgd, #the cols you want to pivot. This says select the temp to percent SGD cols
               names_to = "Variables", #the names of the new cols with all the column names
               values_to = "Values") #names of the new column with all the values

#4.
ChemData_clean<-ChemData %>%
  drop_na() %>% #filters out everything that is not a complete row
  separate(col = Tide_time, #choose the tide time col
           into = c("Tide","Time"), #separate it into two columns Tide and time
           sep = "_", #separate by _
           remove = F) %>%
  pivot_longer(cols = Temp_in:percent_sgd, #the cols you want to pivot. This says select the temp to percent SGD cols  
               names_to = "Variables", #the names of the new cols with all the column names 
               values_to = "Values") %>% #names of the new column with all the values 
  group_by(Variables, Site, Time) %>%
  summarise(mean_vals = mean(Values, na.rm = T))

#5.
ChemData_clean<-ChemData %>%
  drop_na() %>% #filters out everything that is not a complete row
  separate(col = Tide_time, #choose the tide time col
           into = c("Tide","Time"), #separate it into two columns Tide and time
           sep = "_", #separate by _
           remove = F) %>%
  pivot_longer(cols = Temp_in:percent_sgd, #the cols you want to pivot. This says select the temp to percent SGD cols  
               names_to = "Variables", #the names of the new cols with all the column names 
               values_to = "Values") %>% #names of the new column with all the values 
  group_by(Variables, Site, Time) %>% 
  summarise(mean_vals = mean(Values, na.rm = T)) %>%
  pivot_wider(names_from = Variables,
              values_from = mean_vals) #notice it is now mean_vals as the col name

#6.
ChemData_clean<-ChemData %>%
  drop_na() %>% #filters out everything that is not a complete row
  separate(col = Tide_time, #choose the tide time col
           into = c("Tide","Time"), #separate it into two columns Tide and time
           sep = "_", #separate by _
           remove = F) %>%
  pivot_longer(cols = Temp_in:percent_sgd, #the cols you want to pivot. This says select the temp to percent SGD cols  
               names_to = "Variables", #the names of the new cols with all the column names 
               values_to = "Values") %>% #names of the new column with all the values 
  group_by(Variables, Site, Time) %>% 
  summarise(mean_vals = mean(Values, na.rm = T)) %>%
  pivot_wider(names_from = Variables, 
              values_from = mean_vals) %>% #notice it is now mean_vals as the col name
  write_csv(here("Week_04","Outputs","summary.csv"))  #export as a csv to the right folder


ggplot(ChemData) + #create plot with cleaned data
  geom_bernie(aes(x = Salinity, y = NN), bernie = "sitting") #specify axes and make points a sitting bernie


