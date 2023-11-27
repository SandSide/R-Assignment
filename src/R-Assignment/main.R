library(tidyverse)

# Read data
google_playstore <- read.csv('Google-Playstore.csv') %>%
  select(-App.Id, -Currency, -Minimum.Android, -Developer.Website, -Developer.Email, -Privacy.Policy, -Scraped.Time)

# Find all categories 
categories <- google_playstore %>% select(Category) %>% distinct(Category)

gaming_categories <- c('Action', 'Adventure', 'Arcade', 'Board', 'Casual', 'Puzzle', 'Racing', 'Role Playing', 'Strategy', 'Trivia', 'Word')



