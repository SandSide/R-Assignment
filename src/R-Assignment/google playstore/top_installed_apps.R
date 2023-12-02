# Find top 1000 apps by installs
top_1000_installed_apps <- google_playstore %>%
  top_n(1000, Maximum.Installs)
  
# How much are top 1000 apps of total installs
top_1000_installed_apps %>%
  summarise(perc_of_total_installs = sum(Maximum.Installs)/total_installs * 100)

# How much are top 100 apps of total installs
google_playstore %>%
  top_n(100, Maximum.Installs) %>%
  summarise(perc_of_total_installs = sum(Maximum.Installs)/total_installs * 100)

# How much are top 100 apps of total installs
google_playstore %>%
  top_n(10, Maximum.Installs) %>%
  summarise(perc_of_total_installs = sum(Maximum.Installs)/total_installs * 100)


# Calculate distribution of 
top_1000_installed_apps %>%
  group_by(Category) %>%
  summarise(perc_of_apps = (n()/1000) * 100) %>%
  arrange(desc(perc_of_apps))

# Are all top 1000 apps free
top_1000_installed_apps %>%
  filter(Free == 'False') %>%
  summarise(count = n())

# Find first paid app sorted my installs amount
first_paid_app_by_installs <- google_playstore[which(google_playstore$Free=='False', arr.ind=TRUE)[1],] 
