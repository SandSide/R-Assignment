# This script analyses the databset based on category installs and install ranges (1000+, 10 +)

# Analyse ----

# Sort Installs + based on value
sorted_install_range <- google_playstore %>% 
  distinct(Installs) %>%
  mutate(Installs = gsub('[,+]', '', Installs),
         Installs = as.numeric(Installs)) %>%
  filter(!is.na(Installs)) %>%
  arrange(Installs) %>%
  mutate(Installs = format(Installs, big.mark = ',', scientific = FALSE),
         Installs =  paste0(str_trim(Installs), '+'))

# Count apps amount based on installs
app_count_by_installs <- google_playstore %>%
  group_by(Installs) %>%
  summarise(apps = n(),
            perc_of_apps = (apps/total_apps) * 100 ) %>%
  arrange(Installs)

# Sort based on installs
app_count_by_installs <- app_count_by_installs[match(sorted_install_range$Installs, app_count_by_installs$Installs), ]

# Graphs ----

# Display distribution of apps by installs
google_playstore %>%
  filter(!is.na(Installs)) %>%
  group_by(Installs) %>%
  ggplot(aes(y = factor(Installs, levels = sorted_install_range$Installs))) +
  geom_bar() +
  labs(title = 'Distribution of Apps by Installs',
       x = 'Number of Apps',
       y = 'Installs') +
  theme_minimal()


# Display distribution of apps by installs as a percentage
app_count_by_installs %>%
  filter(!is.na(Installs)) %>%
  ggplot(aes(x = perc_of_apps, y = factor(Installs, levels = sorted_install_range$Installs))) +
  geom_bar(stat = 'identity') +
  labs(title = 'Percentage of Apps by Installs',
       x = 'Percentage of Apps',
       y = 'Installs') +
  theme_minimal()
