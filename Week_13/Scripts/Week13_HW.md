Week 13 Homework
================
Shelbie Ishimaru
2024-12-04

### Load Libraries

``` r
library(tidyverse) #for data manipulation
library(here) #for creating unbreakable file paths
```

### Find Data File Path

``` r
hwpath <- here("Week_13", "Data", "homework") #point to the location on the computer of the data folder
```

### Complete the Homework with Loops

``` r
files <- dir(path= hwpath, pattern= ".csv") #make files all items within the file path that are CSVs

#pre-allocate space
hw_data1 <- tibble(filename =  rep(NA, length(files)),  #column name for file 
                mean_temp = rep(NA, length(files)), #column name for the mean temperature
                sd_temp = rep(NA, length(files)), #column name for the temperature standard deviation
                mean_light = rep(NA, length(files)), #column name for the mean light
                sd_light = rep(NA, length(files)), #column name for the light standard deviation
) 

#create the for loop
for (i in 1:length(files)){ # loop over the number of files 
  raw_data <- read_csv(paste0(hwpath,"/",files[i])) #set raw data to manipulate
  hw_data1$filename[i] <- files[i] #fill file name column
  hw_data1$mean_temp[i] <- mean(raw_data$Temp.C, na.rm =T) #fill temperature mean column
  hw_data1$sd_temp[i] <- sd(raw_data$Temp.C, na.rm =T) #fill temperature sd column
  hw_data1$mean_light[i] <- mean(raw_data$Intensity.lux, na.rm =T) #fill light mean column
  hw_data1$sd_light[i] <- sd(raw_data$Intensity.lux, na.rm =T) #fill light sd column
} 

hw_data1 #look at out final dataframe for part 1
```

    ## # A tibble: 4 × 5
    ##   filename mean_temp sd_temp mean_light sd_light
    ##   <chr>        <dbl>   <dbl>      <dbl>    <dbl>
    ## 1 TP1.csv       13.3    2.32       427.    1661.
    ## 2 TP2.csv       13.2    2.31      5603.   11929.
    ## 3 TP3.csv       13.1    2.32      5605.   12101.
    ## 4 TP4.csv       13.2    2.27       655.    2089.

### Complete the Homework with map()

``` r
files <- dir(path= hwpath, pattern= ".csv", full.names= T) #make files all items within the file path that are CSVs and show the full file path
hw_data2 <- files %>% #files is what we are iterating over
  set_names() %>% #sets the id of each list to the file path
  map_df(read_csv,.id= "filename")  %>% #map everything to a dataframe and put the id in a column called filename
  group_by(filename) %>% #conduct our analysis by different file paths
  summarise(mean_temp= mean(Temp.C, na.rm= T), #find mean temperature for each file path
            sd_temp= sd(Temp.C, na.rm= T), #find temperature standard deviation for each file path
            mean_light= mean(Intensity.lux, na.rm= T), #find mean light for each file path
            sd_light= sd(Intensity.lux, na.rm= T)) #find light standard deviation for each file path

hw_data2 #look at out final dataframe for part 2
```

    ## # A tibble: 4 × 5
    ##   filename                                 mean_temp sd_temp mean_light sd_light
    ##   <chr>                                        <dbl>   <dbl>      <dbl>    <dbl>
    ## 1 /Users/shelbieishimaru/Desktop/Reposito…      13.3    2.32       427.    1661.
    ## 2 /Users/shelbieishimaru/Desktop/Reposito…      13.2    2.31      5603.   11929.
    ## 3 /Users/shelbieishimaru/Desktop/Reposito…      13.1    2.32      5605.   12101.
    ## 4 /Users/shelbieishimaru/Desktop/Reposito…      13.2    2.27       655.    2089.
