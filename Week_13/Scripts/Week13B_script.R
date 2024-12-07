# MBIO 612: Week 13B
# Created by: Shelbie Ishimaru
# Created on: 2024-12-7
################################################################################
# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(here)
library(palmerpenguins)
library(broom)
library(performance) #need see package to be installed for this to run!
library(modelsummary)
library(tidymodels)

# Modeling the Penguin Data set ------------------------------------------------
#linear model of Bill depth ~ Bill length by species
#is the slope changing across these 3 species?
Peng_mod<-lm(bill_length_mm ~ bill_depth_mm*species, data = penguins)

check_model(Peng_mod) # check assumptions of an lm model, performance package visualizes every important assumption

anova(Peng_mod) #base R look at the results, p values show significant relationships 
summary(Peng_mod) #shows coefficients of effect size 

#tidy coefficients with the broom package
coeffs<-tidy(Peng_mod) # just put tidy() around it, clean and doesnt show significance stars
coeffs #makes a clean tibble you can work with/export NOT just look at

#tidy r2, etc with the broom package
results<-glance(Peng_mod) #model level stats
results

# tidy residuals, etc with the broom package
resid_fitted<-augment(Peng_mod) #gives fitted predictions so you dont have to use geom_smooth
resid_fitted #helpful for plotting

# Results in modelsummary ------------------------------------------------------
#comparing 2 models
#new model
Peng_mod_noX<-lm(bill_length_mm ~ bill_depth_mm, data = penguins)
#make a list of models and name them
models<-list("Model with interaction" = Peng_mod,
             "Model with no interaction" = Peng_mod_noX)
#save the results as a .docx
modelsummary(models, output = here("Week_13","output","table.docx")) #creates a table that is formatted in word (easy to polish for pubs)

#make coefficient plot, can layer it (w/ +) like a ggplot 
library(wesanderson)
modelplot(models) + #can compare many models in ne plot!
  labs(x = 'Coefficients', 
       y = 'Term names') +
  scale_color_manual(values = wes_palette('Darjeeling1'))

# Comparing many models for a lot of parameters --------------------------------
#use purr, dplyr, and broom! Then make a set of lists and run a model for each subset
models<- penguins %>%
  ungroup()%>% # the penguin data are grouped so we need to ungroup them, make it clean
  nest(.by = species) %>% # nest all the data by species
  mutate(fit = map(data, ~lm(bill_length_mm~body_mass_g, data = .))) #iterates a process for every line of this tibble 
models
models$fit #shows you each of the 3 models

#view the results
results<-models %>%
  mutate(coeffs = map(fit, tidy), # look at the coefficients iteratively (maps fit data with the function tidy)
         modelresults = map(fit, glance)) %>% # R2 and others (maps fit data with the function glance)
  #select what we want to show and unnest it to bring it back to a dataframe
  select(species, coeffs, modelresults) %>% # only keep the results or else the table will be too long!
  unnest() # put it back in a dataframe and specify which columns to unnest
results

# tidymodels -------------------------------------------------------------------
lm_mod<-linear_reg() %>% #running a linear regression model
  set_engine("lm") %>% #pipe to an engine (what type of linear regression, going with the classic OLS)
  fit(bill_length_mm ~ bill_depth_mm*species, data = penguins) %>% #put a fit into the model 
  tidy() %>% #broom package to get the summary stats
  ggplot()+ #pipe to plot!
  geom_point(aes(x = term, y = estimate))+ #points 
  geom_errorbar(aes(x = term, ymin = estimate-std.error, #add error bars 
                    ymax = estimate+std.error), width = 0.1 )+
  coord_flip() #change coords to the visuals we want
lm_mod #see plot results

# totally awesome R package ----------------------------------------------------
#sends you a notification to yuor phone when your code is done running!
#install.packages("pushoverr")
library(pushoverr)
pushover("your code is done!")