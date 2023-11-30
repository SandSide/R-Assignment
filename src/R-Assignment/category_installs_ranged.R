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
  filter(install_range == 'Low') %>%
  ggplot(aes(x = median_installs , y = reorder(Category, median_installs ), fill = Category)) +
  geom_bar(stat = 'identity', width = 1, color = 'white') +
  labs(title = 'Median number of installs per category in low install range', x = 'Median Number of Installs', y = 'Category', fill = 'Category') +
  theme_minimal() + 
  guides(fill = 'none')

# For each install, range figure out % more or less from median
category_installs_ranged %>% 
  filter(install_range == 'Low') %>%
  mutate(min_range_diff = (median_installs/100000) * 100)

# For each install, range figure out % more or less from median
category_installs_ranged %>% 
  filter(install_range == 'Medium') %>%
  mutate(min_range_diff = (median_installs - 100000)/100000 * 100)

category_installs_ranged %>% 
  filter(install_range == 'High') %>%
  mutate(min_range_diff = (median_installs - 1000000)/1000000 * 100)

category_installs_ranged %>% 
  filter(install_range == 'Very High') %>%
  mutate(min_range_diff = (avg_installs - 10000000)/10000000 * 100)



# ggplot(category_installs_ranged, aes(x = install_range, y = avg_installs, fill = Category)) +
#   geom_bar(stat = 'identity', position = 'dodge') +
#   labs(title = 'Average Maximum Installs by Install Range and Category', x = 'Install Range', y = 'Average Maximum Installs') +
#   theme_minimal()