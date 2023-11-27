# Find total installs
total_installs <- sum(google_playstore$Maximum.Installs)

# Find install total for each category
category_installs <- google_playstore %>% 
  group_by(Category) %>% 
  summarise(installs = sum(Maximum.Installs))

# Calculate installs as a percentage of total installs
category_installs <- category_installs %>% 
  mutate(perc = (installs/total_installs) * 100) %>% 
  mutate(perc = round(perc, 2))

# Display as a vertical bar graph
category_installs %>% 
  ggplot(aes(x = perc, y = reorder(Category, perc), fill = Category)) +
  geom_bar(stat = 'identity', width = 1, color = 'white') +
  labs(title = 'Total Installs per Category as a Percentage of Total Installs', x = 'Percentage of Installs', y = 'Category', fill = 'Category') +
  theme_minimal() + 
  guides(fill = 'none')

# Summarise data
category_installs_summary <- category_installs %>%
  summarise(avg_installs = mean(installs), avg_perc = mean(perc), median_installs = median(installs), median_perc = median(perc))



