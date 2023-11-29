# Find total installs
total_installs <- sum(google_playstore$Maximum.Installs)

# Find install total for each category
category_installs <- google_playstore %>% 
  group_by(Category) %>% 
  summarise(installs = sum(Maximum.Installs), avg_installs = mean(Maximum.Installs), median_installs = median(Maximum.Installs))

# Calculate installs as a percentage of total installs
category_installs <- category_installs %>% 
  mutate(perc = (installs/total_installs) * 100) %>% 
  mutate(perc = round(perc, 2))

# Summaries data
category_installs_summary <- category_installs %>%
  summarise(avg_installs = mean(installs), avg_perc = mean(perc), median_installs = median(installs), median_perc = median(perc))



# Display as a vertical bar graph
category_installs %>% 
  ggplot(aes(x = perc, y = reorder(Category, perc), fill = Category)) +
  geom_bar(stat = 'identity', width = 1, color = 'white') +
  labs(title = 'Total Installs per Category as a Percentage of Total Installs', x = 'Percentage of Installs', y = 'Category', fill = 'Category') +
  theme_minimal() + 
  guides(fill = 'none')

# Display median installs per category
category_installs %>% 
  ggplot(aes(x = median_installs, y = reorder(Category, median_installs), fill = Category)) +
  geom_bar(stat = 'identity', width = 1, color = 'white') +
  labs(title = 'Median number of installs per category', x = 'Median Number of Installs', y = 'Category', fill = 'Category') +
  theme_minimal() + 
  guides(fill = 'none')

# Display average installs per category
category_installs %>% 
  ggplot(aes(x = avg_installs, y = reorder(Category, avg_installs), fill = Category)) +
  geom_bar(stat = 'identity', width = 1, color = 'white') +
  labs(title = 'Average number of installs per category', x = 'Average Number of Installs', y = 'Category', fill = 'Category') +
  theme_minimal() + 
  guides(fill = 'none')

# Define install ranges
install_ranges <- c(0, 100000, 1000000, 10000000, Inf)
install_ranges_labels <- c('Low', 'Medium', 'High', 'Very High')

# Group based on categories and install ranges in the category
category_installs_ranged <- google_playstore %>%
  select(Category, Maximum.Installs) %>%
  mutate(install_range = cut(Maximum.Installs, breaks = install_ranges, labels = install_ranges_labels, include.lowest = TRUE)) %>%
  group_by(Category, install_range) %>%
  summarise(total_installs = sum(Maximum.Installs), avg_installs = mean(Maximum.Installs), median_installs = median(Maximum.Installs))
    
options(scipen = 999)

# Display X range of installed apps
category_installs_ranged %>% 
  filter(install_range == 'High') %>%
  ggplot(aes(x = median_installs , y = reorder(Category, median_installs ), fill = Category)) +
  geom_bar(stat = 'identity', width = 1, color = 'white') +
  labs(title = 'Median number of installs per category in low install range', x = 'Median Number of Installs', y = 'Category', fill = 'Category') +
  theme_minimal() + 
  guides(fill = 'none')


options(scipen = 0)


# ggplot(category_installs_ranged, aes(x = install_range, y = avg_installs, fill = Category)) +
#   geom_bar(stat = 'identity', position = 'dodge') +
#   labs(title = 'Average Maximum Installs by Install Range and Category', x = 'Install Range', y = 'Average Maximum Installs') +
#   theme_minimal()

  





