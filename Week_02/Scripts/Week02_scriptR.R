# MBIO 612: Week 2, Learning to import data and set up file structures
# Created by: Shelbie Ishimaru
# Created on: 20204-09-07
################################################################################
# Load Libraries ---------------------------------------------------------------
library(tidyverse) #load in the tidyverse package to support data manipulation
library(here) #load in the here package to create unbreakable file paths

# Read-in Data -----------------------------------------------------------------
weightdata <- read_csv(here("Week_02", "Data", "weightdata.csv")) #load in weight data 

# Data Analysis -----------------------------------------------------------------
head(weightdata) #shows the first six lines of the df
tail(weightdata) #shows the last six lines of the df
View(weightdata) #opens a new window to see the entire df