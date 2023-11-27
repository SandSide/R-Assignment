library(tidyverse)
library(ggplot2)

# Read data
google_playstore <- read.csv('Google-Playstore.csv')

# Find all categories 
categories <- google_playstore %>% select(Category) %>% distinct(Category)

# Find total installs
total_installs <- sum(google_playstore$Maximum.Installs)


# Find top 100 apps by middle value between max and min installs
# top_100_installed_apps <- google_playstore %>% 
#                             arrange(desc((Maximum.Installs - Minimum.Installs)/2 + Minimum.Installs)) %>% 
#                             slice_head(n = 100)



# Checking if total installs percentage = 100%
# app_installs_by_category %>% summarise(temp = sum(perc))

# Find top N installed apps by percentage. 
# Sum categories not in top N.
# Add Other to the bottom
# top_n_installed_categories <- app_installs_by_category %>% 
#   arrange(desc(perc)) %>%
#   mutate(Category = ifelse(row_number() <= 10, Category, 'Other')) %>%
#   group_by(Category) %>%
#   summarise(perc = sum(perc)) %>%
#   arrange(Category == 'Other', desc(perc))
