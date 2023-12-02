
# Calculate how many free and paids exists, and their ratio the ratio
free_paid_apps <- google_playstore %>%
  summarise(paid_apps = sum(Free == 'False'), 
            free_apps = sum(Free == 'True'), 
            free_to_paid_ratio = free_apps/paid_apps)


free_paid_category <- google_playstore %>%
  group_by(Category) %>%
  summarise(paid_apps = sum(Free == 'False'), 
            free_apps = sum(Free == 'True'), 
            free_to_paid_ratio = free_apps / paid_apps, 
            free_perc = (free_apps/n()) * 100,
            paid_perc = (paid_apps/n()) * 100) %>%
  arrange(desc(paid_apps))


free_paid_category %>% 
  ggplot(aes(x = paid_apps, y = free_apps, color = Category)) +
  geom_point()


google_playstore %>%
  group_by(Free) %>%
  summarise(total_in_app_purchase_apps = sum(In.App.Purchases == 'True')) %>%
  arrange(desc(total_in_app_purchase_apps))