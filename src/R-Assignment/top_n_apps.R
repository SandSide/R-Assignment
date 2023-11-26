library(tidyverse)
library(ggplot2)

# Read data
google_playstore <- read.csv('Google-Playstore.csv')

# Find all categories 
categories <- google_playstore %>% select(Category) %>% distinct(Category)

# Find total installs
total_installs <- sum(google_playstore$Maximum.Installs)


# Find top 100 apps by middle value between max and min installs
top_100_installed_apps <- google_playstore %>% 
                            arrange(desc((Maximum.Installs - Minimum.Installs)/2 + Minimum.Installs)) %>% 
                            slice_head(n = 100)

# Find install total for each category
app_installs_by_category <- google_playstore %>% 
  group_by(Category) %>% 
  summarise(installs = sum(Maximum.Installs))

# Calculate installs as a percentage of total installs
app_installs_by_category <- app_installs_by_category %>% 
  mutate(perc = (installs/total_installs) * 100) %>% 
  mutate(perc = round(perc, 2))

# Checking if total installs percentage = 100%
app_installs_by_category %>% summarise(temp = sum(perc))

# Find top N installed apps by percentage. 
# Sum categories not in top N.
# Add Other to the bottom
top_n_installed_categories <- app_installs_by_category %>% 
  arrange(desc(perc)) %>%
  mutate(Category = ifelse(row_number() <= 10, Category, 'Other')) %>%
  group_by(Category) %>%
  summarise(perc = sum(perc)) %>%
  arrange(Category == 'Other', desc(perc))



# Display as a pie chart
ggplot(top_n_installed_categories, aes(x = '', y = perc, fill = Category)) +
  geom_bar(stat = 'identity', width = 1, color = 'white') +
  coord_polar('y', start = 0) + 
  geom_text(aes(label = sprintf('%.0f%%', perc)),
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(title = 'Top 10 Installed Categories') +
  theme_void() 

# Dispaly as a bar graph
top_n_installed_categories %>% 
  slice_head(n = 10) %>%
  ggplot(aes(x = factor(Category, levels = Category), y = perc, fill = factor(Category, levels = Category))) +
  geom_bar(stat = 'identity', width = 1, color = 'white') +
  labs(title = 'Top 10 Installed Categories', x = 'Category', y = 'Percentage', fill = 'Category') +
  theme_minimal() +
  theme(axis.text.x = element_blank())
