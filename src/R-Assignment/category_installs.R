# Find total installs
total_installs <- sum(google_playstore$Maximum.Installs)

# Find install total for each category
category_installs <- google_playstore %>% 
  group_by(Category) %>% 
  summarise(installs = sum(Maximum.Installs), avg_installs = mean(Maximum.Installs), median_installs = median(Maximum.Installs)) %>%
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
