install_ranges <- c('0+', '1+', '5')

app_count_by_installs <- google_playstore %>%
  group_by(Installs) %>%
  summarise(total_apps = n()) %>%
  arrange(Installs)

app_count_by_installs %>%
  ggplot(aes(y = Installs)) +
  geom_bar()

# Sort Installs+ based on value
sorted_install_range <- google_playstore %>% 
  distinct(Installs) %>%
  mutate(Installs = gsub('[,+]', '', Installs),
         Installs = as.numeric(Installs)) %>%
  arrange(Installs) %>%
  mutate(Installs = format(Installs, big.mark = ",", scientific = FALSE),
         Installs =  paste0(Installs, "+"))
  
  


