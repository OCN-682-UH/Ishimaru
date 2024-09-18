# MBIO 612: Week 4A
# Created by: Shelbie Ishimaru
# Created on: 2024-09-17
################################################################################
# Load Libraries ---------------------------------------------------------------
library(palmerpenguins) #so we can load in our penguin data
library(tidyverse) #for data manipulation
library(here) #used to create unbreakable file paths
library(dadjoke) 

# Read-in Data -----------------------------------------------------------------
glimpse(penguins) #gives us a column overview, shows first 10 lines of df

# Data Analysis: Dplyr --------------------------------------------------
#1. Filter
head(penguins) #look at data before filtering
filter(.data= penguins, sex== "female") #filter out everything but female penguin data (removes NAs)
filter(.data= penguins, year== "2008") #filter out all data so its only from 2008
filter(.data= penguins, body_mass_g > 5000) #filter out all data so the body mass is greater than 5000
filter(.data= penguins, sex== "female", body_mass_g > 5000) #filtering to get data of females who are large
filter(.data= penguins, sex== "female" & body_mass_g > 5000) #filtering to get data of females who are large
filter(.data= penguins, year== "2008" | year== "2009")
filter(.data= penguins, island != "Dream")
filter(.data= penguins, species %in% c("Adelie", "Gentoo"))

#2. Mutate
data_2 <- mutate(.data= penguins, body_mass_kg = body_mass_g/1000)
data_2 <- mutate(.data= penguins, body_mass_kg = body_mass_g/1000, bill_length_depth= bill_length_mm/bill_depth_mm)
data_3 <- mutate(.data= penguins, fliiper_body = body_mass_g + flipper_length_mm)

#3. ifelse
data_2 <- mutate(.data= penguins, after_2008= ifelse(year> 2008, "After 2008", "Before 2008")) #case_when() is for more than 2 cases
data_3 <- mutate(.data= penguins, fliiper_body = body_mass_g + flipper_length_mm, 
                 size= ifelse(body_mass_g> 4000, "Large", "Small"))

#4. Pipe operators
penguins %>% filter(sex=="female") %>%
  mutate(log_mass= log(body_mass_g))

#5. Select
penguins %>% filter(sex=="female") %>%
  mutate(log_mass= log(body_mass_g)) %>%
  select(species, island, sex, log_mass) #select out the cols you want to keep 

penguins %>% filter(sex=="female") %>%
  mutate(log_mass= log(body_mass_g)) %>%
  select(Species= species, island, sex, log_mass) #same select but also rename the species col 

#6. Summarize
penguins %>% summarize(mean_flipper= mean(flipper_length_mm, na.rm= T)) #calculate summary stats, removes NAs from the dataset

penguins %>% summarize(mean_flipper= mean(flipper_length_mm, na.rm= T),
                       min_flipper= min(flipper_length_mm, na.rm= T))

penguins %>% group_by(island) %>% summarize(mean_flipper= mean(flipper_length_mm, na.rm= T),
                                            min_flipper= min(flipper_length_mm, na.rm= T))

penguins %>% group_by(island, sex) %>% summarize(mean_flipper= mean(flipper_length_mm, na.rm= T), 
                                                 min_flipper= min(flipper_length_mm, na.rm= T))

#7. Drop_NAs
penguins %>% drop_na(sex) %>% 
  group_by(island, sex) %>%
  summarize(mean_flipper= mean(flipper_length_mm, na.rm= T), 
            min_flipper= min(flipper_length_mm, na.rm= T))#only drop missing data from sex col

#8. Pipe to ggplot
penguins %>% drop_na(sex) %>% ggplot(aes(x= sex, y= flipper_length_mm)) + geom_boxplot() #drop NAs in sex col then plot 

#9. Dad Joke
dadjoke() #add a fun dad joke :)
