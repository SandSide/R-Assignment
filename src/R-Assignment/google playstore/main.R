# This is my main script. It contains a function to read the csv file and transform it into a suitable format.
# It also contains functions used throughout my project.

# Utility Functions ----

# Basic setup 
set_up <- function(){
  library(tidyverse)
  options(scipen = 999)
}

# Read data set from the csv file
get_dataset <- function(){
  
  # Read data
  google_playstore <- read.csv('google playstore/Google-Playstore.csv') %>%
    select(-App.Id, -Currency, -Minimum.Android, -Developer.Website, -Developer.Email, -Privacy.Policy, -Scraped.Time)
  
  # A group of categories apart of gaming category
  gaming_categories <- c('Action', 'Adventure', 'Arcade', 'Board', 'Casual', 'Card', 'Puzzle', 'Racing', 'Role Playing', 'Simulation', 'Sports', 'Strategy', 'Trivia', 'Word')
  
  # Group all gaming categories under gaming category
  google_playstore <- google_playstore %>%
    mutate(Category = if_else(Category %in% gaming_categories, 'Gaming', Category))

  return(google_playstore)
  
}

# Convert installs (ex. 100,000+) into a number
installs_to_num <- function(installs){

  result <- as.numeric(gsub('[,+]', '', installs))
  return(result)
}

# Convert number into installs (ex. 100,000+)
num_to_installs <- function(num){
  
  result <- prettyNum(num, big.mark = ',', scientific = FALSE)
  result <- paste0(str_trim(result), '+')
    
  return(result)
}

# Calculate middle number between min and max
calculate_mid_num <- function(min, max){
  return ((max - min)/2) + min; 
}


# Start of the Project ----

# Get the data set
google_playstore <- get_dataset()


# Find all categories 
categories <- google_playstore %>% select(Category) %>% distinct(Category) 

# Calcualte number of apps in the dataset
total_apps <- nrow(google_playstore)

# Find total installs using max installs
total_installs <- sum(google_playstore$Maximum.Installs)

# Find a total installs using approximation
total_installs <- google_playstore %>%
  mutate(installs_arpox = calculate_mid_num(Minimum.Installs, Maximum.Installs)) %>%
  filter(!is.na(installs_arpox)) %>%
  summarise(total_installs = sum(installs_arpox)) %>%
  pull(total_installs)
  
