

# Find top 1000 apps by installs
top_1000_installed_apps <- google_playstore %>%
  mutate(aprox_installs = calculate_mid_num(Minimum.Installs, Maximum.Installs)) %>%
  arrange(desc(aprox_installs)) %>%
  slice_head(n = 1000)


# Find top 10 apps by installs
top_10_installed_apps <- top_1000_installed_apps %>%
  slice_head(n = 10) %>%
  mutate(installs_aprox = calculate_mid_num(Minimum.Installs, Maximum.Installs),
         installs_aprox = prettyNum(installs_aprox, big.mark = ',', scientific = FALSE)) %>%
  select(App.Name, installs_aprox, Developer.Id)
  
# Calculate distribution of top 1000 apps by category as a percentage
distribution_of_top_1000_installed_apps_by_category <- top_1000_installed_apps %>%
  group_by(Category) %>%
  summarise(perc_of_apps = (n()/1000) * 100) %>%
  arrange(desc(perc_of_apps))


# Find categorizes not in top 1000 installed apps
categories %>%
  filter(!(categories$Category %in% top_1000_installed_apps$Category))

# Find amout of apps that are Editors Choice in top 1000
top_1000_installed_apps %>%
  group_by(Editors.Choice) %>%
  summarise(amount = n())


# Display distribution by category as a bar graph
distribution_of_top_1000_installed_apps_by_category %>%
  ggplot(aes(x = perc_of_apps, y = reorder(Category, perc_of_apps), fill = Category)) +
  geom_bar(stat = 'identity', width = 1, color = 'white') +
  labs(title = 'Distribution of Apps by Category as a Percentage in Top 1000 Apps by Installs', x = 'Percentage of Apps', y = 'Category', fill = 'Category') +
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



# Find first paid app sorted by max installs
first_paid_app <- google_playstore[which(google_playstore$Free == 'False')[1], ]

         