# Find top 1000 apps by installs
top_1000_installed_apps <- google_playstore %>%
  top_n(1000, Maximum.Installs)
  
# Calculate distribution of 
distribution_of_top_1000_installed_apps_by_category <- top_1000_installed_apps %>%
  group_by(Category) %>%
  summarise(perc_of_apps = (n()/1000) * 100) %>%
  arrange(desc(perc_of_apps))

distribution_of_top_1000_installed_apps_by_category %>%
  ggplot(aes(x = perc_of_apps, y = reorder(Category, perc_of_apps), fill = Category)) +
  geom_bar(stat = 'identity', width = 1, color = 'white') +
  labs(title = 'Distribution of Apps by Category in Top 1000 Apps by Installs', x = 'Frequency', y = 'Category', fill = 'Category') +
  theme_minimal() + 
  guides(fill = 'none')

# Are all top 1000 apps free
top_1000_installed_apps %>%
  filter(Free == 'False') %>%
  summarise(count = n())

# Find how much top apps are of total installs
top_1000_stats <- data.frame(
  installed_apps = c('Top 1000', 'Top 100', 'Top 10'),
  perc_of_total_installs = c(
    google_playstore %>% top_n(1000, Maximum.Installs) %>% summarise(perc_of_total_installs = sum(Maximum.Installs) / total_installs * 100) %>% pull(perc_of_total_installs),
    google_playstore %>% top_n(100, Maximum.Installs) %>% summarise(perc_of_total_installs = sum(Maximum.Installs) / total_installs * 100) %>% pull(perc_of_total_installs),
    google_playstore %>% top_n(10, Maximum.Installs) %>% summarise(perc_of_total_installs = sum(Maximum.Installs) / total_installs * 100) %>% pull(perc_of_total_installs)
  )
)

# Plotting the results
ggplot(top_1000_stats, aes(x = installed_apps, y = perc_of_total_installs, fill = installed_apps)) +
  geom_bar(stat = 'identity', color = 'black', alpha = 0.7) +
  labs(title = 'Percentage of Total Installs of Top Installed Apps',
       x = 'Top Installed Apps',
       y = 'Percentage of Total Installs') +
  theme_minimal() + 
  guides(fill = 'none')


# Find first paid app sorted my installs amount
first_paid_app_by_installs <- google_playstore %>%
  filter(Free == 'False') %>%
  arrange(desc(Maximum.Installs)) %>%
  slice(1)
