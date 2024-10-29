# MBIO 612: Week 9B
# Created by: Shelbie Ishimaru
# Created on: 2024-10-26
################################################################################
# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(here)

# Load in Data -----------------------------------------------------------------
#tuesdata <- tidytuesdayR::tt_load(2021, week = 7)
#income_mean<-tuesdata$income_mean
income_mean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_mean.csv')

glimpse(starwars) #starwars from forcats package

# What is a Factor -------------------------------------------------------------
fruits<-factor(c("Apple", "Grape", "Banana")) #default levels are alphabetical and they become stored as an integer
fruits 

test<-c("A", "1", "2") 
as.numeric(test)

test<-factor(test) # covert to factor
as.numeric(test) #will change A to a 3 NOT NA (very bad)
#This is why we should use read_csv() NOT read.csv() 

# Reordering Factors -----------------------------------------------------------
starwars %>% #how many species are present throught the film
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) #sort by greatest the least

star_counts<-starwars %>% #lump every rare species that only had one into an other category
  filter(!is.na(species)) %>%
  mutate(species = fct_lump(species, n = 3)) %>% #lumping species that have n<3 are lumped into other
  count(species)
star_counts

star_counts %>% #make a basic plot!
  ggplot(aes(x = species, y = n))+
  geom_col()

star_counts %>% #sorted from lowest to highest
  ggplot(aes(x = fct_reorder(species, n), y = n))+ # reorder the factor of species by n
  geom_col()

star_counts %>%  #sorted from highest to lowest
  ggplot(aes(x = fct_reorder(species, n, .desc = TRUE), y = n))+ # reorder the factor of species by n
  geom_col() +
  labs(x = "Species")

# Reordering Line Plots --------------------------------------------------------
glimpse(income_mean)

total_income<-income_mean %>%
  group_by(year, income_quintile)%>%
  summarise(income_dollars_sum = sum(income_dollars))%>%
  mutate(income_quintile = factor(income_quintile)) # make it a factor

total_income%>% #how income changes overtime in different quintiles
  ggplot(aes(x = year, y = income_dollars_sum, color = income_quintile))+
  geom_line()

total_income%>% #reorders the data so the legend in in order
  ggplot(aes(x = year, y = income_dollars_sum, 
             color = fct_reorder2(income_quintile,year,income_dollars_sum)))+
  geom_line()+
  labs(color = "income quantile")

# Reordering levels in a vector ------------------------------------------------
#did not use lubridate so R doesn't know its a date
x1 <- factor(c("Jan", "Mar", "Apr", "Dec"))
x1 #default= alphabetical

x1 <- factor(c("Jan", "Mar", "Apr", "Dec"), levels = c("Jan", "Mar", "Apr", "Dec"))
x1 #set specific order of levels

# Subset data with factors -----------------------------------------------------
starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% # make species a factor
  filter(n>3) # only keep species that have more than 3
starwars_clean

levels(starwars_clean$species) #look at levels

starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% # make species a factor 
  filter(n>3)  %>% # only keep species that have more than 3 
  droplevels() # drop extra levels

levels(starwars_clean$species) #look at levels again

starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% # make species a factor 
  filter(n>3)  %>% # only keep species that have more than 3 
  droplevels() %>% # drop extra levels 
  mutate(species = fct_recode(species, "Humanoid" = "Human")) #rename a level
starwars_clean

# Fun R Package ----------------------------------------------------------------
install.packages('gm')
library(gm)
m <- 
  Music() +  # initialize a Music object
  Meter(4, 4) + # add a 4/4 time signature
  Line(c("C5", "D5", "E5", "F5")) # add a musical line of four quarter notes
gm::show(m, to = c("score", "audio"))
