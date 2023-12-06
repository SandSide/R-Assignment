# This script analyses installs 

# Graphs ----

# Display distribution of apps by installs
google_playstore %>%
  filter(!is.na(Installs),
         installs_to_num(Installs) < 50000000) %>%
  group_by(Installs) %>%
  ggplot(aes(y = factor(Installs, levels = sorted_install_range$Installs))) +
  geom_bar(color = 4, fill = 'white') +
  labs(title = 'Distribution of Apps by Installs',
       x = 'Number of Apps',
       y = 'Installs')
