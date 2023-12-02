
# Calculate how many free and paids exists, and their ratio the ratio
free_paid_apps <- google_playstore %>%
  summarise(paid_apps = sum(Free == 'False'), 
            free_apps = sum(Free == 'True'), 
            free_to_paid_ratio = free_apps/paid_apps)

# Calculate ratio of fre and paids apps by category
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


# Find how many free and paid apps ahve in game purchases
google_playstore %>%
  group_by(Free) %>%
  summarise(total_in_app_purchase_apps = sum(In.App.Purchases == 'True')) %>%
  arrange(desc(total_in_app_purchase_apps))

# Calculate by category how many apps are free/paid & have/dont have in game purchases
free_paid_purchase_category <- google_playstore %>%
  group_by(Category) %>%
  summarise(free_purchase = sum(Free == 'True' & In.App.Purchases == 'True'),
            paid_purchase = sum(Free == 'False' & In.App.Purchases == 'True'),
            free_purchase_free = sum(Free == 'True' & In.App.Purchases == 'False'),
            paid_purchase_free = sum(Free == 'False' & In.App.Purchases == 'False')) %>%
  arrange(desc(free_purchase_free))

free_paid_purchase_category %>% 
  ggplot(aes(x = free_purchase, y = free_purchase_free)) +
  geom_point()

free_paid_purchase_category %>% 
  ggplot(aes(x = free_purchase_free, y = paid_purchase_free)) +
  geom_point()

