# Find total installs
total_installs <- sum(google_playstore$Maximum.Installs)

# Find install total for each category
app_installs_by_category <- google_playstore %>% 
  group_by(Category) %>% 
  summarise(installs = sum(Maximum.Installs))

# Calculate installs as a percentage of total installs
app_installs_by_category <- app_installs_by_category %>% 
  mutate(perc = (installs/total_installs) * 100) %>% 
  mutate(perc = round(perc, 2))

# Display as a bar graph
app_installs_by_category %>% 
  ggplot(aes(x = perc, y = reorder(Category, perc), fill = Category)) +
  geom_bar(stat = 'identity', width = 1, color = 'white') +
  labs(title = 'Total Installs per Category as a Percentage of Total Installs', x = 'Percentage of Installs', y = 'Category', fill = 'Category') +
  theme_minimal() + 
  guides(fill = 'none')



