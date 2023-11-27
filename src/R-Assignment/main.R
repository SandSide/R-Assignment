library(tidyverse)
library(ggplot2)

# Read data
google_playstore <- read.csv('Google-Playstore.csv')

# Find all categories 
categories <- google_playstore %>% select(Category) %>% distinct(Category)


