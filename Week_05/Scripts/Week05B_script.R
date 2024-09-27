# MBIO 612: Week 5B
# Created by: Shelbie Ishimaru
# Created on: 2024-09-24
################################################################################
# Load Libraries ---------------------------------------------------------------
library(tidyverse)
library(here)
library(lubridate) #package to deal with dates and times

# Load Data ---------------------------------------------------------------
#Environmental data from each site
CondData<-read_csv(here("Week_05","data", "CondData.csv"))

#look at data
glimpse(CondData)

#Working with lubridate <- dates MUST be a character!
now() #what time is it now?
now(tzone = "EST") #what time is it on the east coast
now(tzone = "GMT") #what time in GMT
today() #what is todays date
today(tzone = "GMT") #what is todays date in GMT
am(now()) #is it morning? (gives bool)
leap_year(now()) #is it a leap year? (gives bool)

ymd("2021-02-24") #converts to ISO date in the specified order
mdy("02/24/2021") #converts to ISO date in the specified order
mdy("February 24 2021") #converts to ISO date in the specified order
dmy("24/02/2021") #converts to ISO date in the specified order

#NOTE: is you use _hms you need to account for seconds!!
ymd_hms("2021-02-24 10:22:20 PM") #converts to ISO datetime in the specified order
mdy_hms("02/24/2021 22:22:20") #assumes military time if you dont write AM or PM
mdy_hm("February 24 2021 10:22 PM") #converts to ISO datetime in the specified order

#create a vector of dates
datetimes<-c("02/24/2021 22:22:20", #keep the same format within a vector (so all data collected/in excel must be the same!)
             "02/25/2021 11:21:10",
             "02/26/2021 8:01:52")

datetimes <- mdy_hms(datetimes)  #converts to ISO datetime in the specified order (month, day, year)

month(datetimes) #tells us the month of each date in the vector
month(datetimes, label = TRUE) #provides the name of the month instead of number, now in a factor (good for plotting)!
month(datetimes, label = TRUE, abbr = FALSE) #spells it out the whole month, still a factor
day(datetimes) #extract day
wday(datetimes, label = TRUE) #extract day of week, in a factor 
hour(datetimes) #extract hours
minute(datetimes) #extract mins
second(datetimes) #extract secs

datetimes + hours(4) #this adds 4 hours to each datetime, NOTE: hours() with the S
datetimes + days(2) #this adds 2 days to each datetime, NOTE: days() with the S

round_date(datetimes, "minute") #rounds datetimes to nearest minute
round_date(datetimes, "5 mins") # round datetimes to nearest 5 minute

#CatterPlots Package
library(CatterPlots)
x <-c(1:10) #make up some data
y<-c(1:10) #make up some more data
catplot(xs=x, ys=y, cat=3, catcolor='blue') #make plot with fake data and use cat plot package!

#Challenge
CondClean <- CondData %>% #read in cond data
  mutate(Datetime= mdy_hms(date)) %>% #create an new column that converts the date column to datetime
  select(Datetime, Temperature, Serial, Salinity) #remove the old date column and order columns
