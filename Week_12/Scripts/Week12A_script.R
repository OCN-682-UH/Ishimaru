# MBIO 612: Week 12
# Created by: Shelbie Ishimaru
# Created on: 2024-11-19
################################################################################
# Load libraries ---------------------------------------------------------------
library(tidyverse)

# Read-in Data -----------------------------------------------------------------
#open csv file, highlight the first 6 lines, copy the highlighted area, In R go to "addins" --> "Paste as Tibble"
data <- tibble::tribble(
  ~lat,    ~long, ~star_no,
  33.548, -117.805,      10,
  35.534, -121.083,       1,
  39.503, -123.743,      25,
  32.863,  -117.24,      22,
  33.46, -117.671,       8L
)

# Create Reprex ----------------------------------------------------------------
#in R select all of the code we have, then go to "addins" --> "Render Reprex..." --> select "current selection" and "Append session info"
mpg %>%
  ggplot(aes(x = displ, y = hwy))%>%
  geom_point(aes(color = class))

# Think, Pair, Share -----------------------------------------------------------
# Load libraries ---------------------------------------------------------------
library(tidyverse)

# Read-in Data -----------------------------------------------------------------
data <- tibble::tribble(
  ~lat,    ~long, ~star_no,
  33.548, -117.805,      10,
  35.534, -121.083,       1,
  39.503, -123.743,      25,
  32.863,  -117.24,      22,
  33.46, -117.671,       8
)

# Create Reprex ----------------------------------------------------------------
plot <- ggplot(data, aes(x= star_num)) +
  geom_histogram()
plot
