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
  summarise(total_apps = n()) %>%
  arrange(Installs)

# Sort based on installs
app_count_by_installs <- app_count_by_installs[match(sorted_install_range$Installs, app_count_by_installs$Installs), ]

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

