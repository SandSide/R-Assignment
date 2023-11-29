library(tidyverse)

# Read data
google_playstore <- read.csv('Google-Playstore.csv') %>%
  select(-App.Id, -Currency, -Minimum.Android, -Developer.Website, -Developer.Email, -Privacy.Policy, -Scraped.Time)

# A group of categoires apart of gaming category
gaming_categories <- c('Action', 'Adventure', 'Arcade', 'Board', 'Casual', 'Card', 'Puzzle', 'Racing', 'Role Playing', 'Simulation', 'Sports', 'Strategy', 'Trivia', 'Word')

# Group all gaming categories udner gaming category
google_playstore <- google_playstore %>%
  mutate(Category = if_else(Category %in% gaming_categories, 'Gaming', Category))

# Find all categories 
categories <- google_playstore %>% select(Category) %>% distinct(Category)