set_up <- function(){
  library(tidyverse)
  options(scipen = 999)
}

get_dataset <- function(){
  
  # Read data
  google_playstore <- read.csv('google playstore/Google-Playstore.csv') %>%
    select(-App.Id, -Currency, -Minimum.Android, -Developer.Website, -Developer.Email, -Privacy.Policy, -Scraped.Time)
  
  # A group of categories apart of gaming category
  gaming_categories <- c('Action', 'Adventure', 'Arcade', 'Board', 'Casual', 'Card', 'Puzzle', 'Racing', 'Role Playing', 'Simulation', 'Sports', 'Strategy', 'Trivia', 'Word')
  
  # Group all gaming categories under gaming category
  google_playstore <- google_playstore %>%
    mutate(Category = if_else(Category %in% gaming_categories, 'Gaming', Category))
  
  # Find all categories 
  categories <- google_playstore %>% select(Category) %>% distinct(Category) 
  
  return(google_playstore)
  
}


range_categories_by_installs <- function(){
  
  # Define install ranges
  install_ranges <- c(0, 100000, 1000000, 10000000, Inf)
  install_ranges_labels <- c('Low', 'Medium', 'High', 'Very High')
  
  # Group based on categories and install ranges in the category
  category_installs_ranged <- google_playstore %>%
    mutate(install_range = cut(Maximum.Installs,
                               breaks = install_ranges, 
                               labels = install_ranges_labels, 
                               include.lowest = TRUE))
  
  return(category_installs_ranged)
}


installs_to_num <- function(installs){
  
  result <- as.numeric(gsub('[,+]', '', installs))

  return(result)
}

num_to_installs <- function(num){
  
  result <- format(num, big.mark = ',', scientific = FALSE)
  result <- paste0(str_trim(result), '+')
    
  return(result)
}

# Store data set
google_playstore <- get_dataset()

total_apps <- nrow(google_playstore)
  


