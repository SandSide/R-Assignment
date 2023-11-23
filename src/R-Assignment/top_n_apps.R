library(tidyverse)
library(ggplot2)
google_playstore <- read.csv('Google-Playstore.csv')

categories <- google_playstore %>% select(Category) %>% distinct(Category)

google_playstore %>% filter(Category == "Puzzle") %>% select(App.Name)

google_playstore %>% 
  group_by(Category) %>% 
  summarize(total_installs = sum(Maximum.Installs))


top_100_installed_apps <- google_playstore %>% 
                            arrange(desc(Maximum.Installs)) %>% 
                            slice_head(n = 100)



## Find install total for each category
app_amount_by_category <- google_playstore %>% 
  group_by(Category) %>% 
  summarise(installs = sum(Maximum.Installs))

# Find total installs
total_installs <- sum(google_playstore$Maximum.Installs)

# Calculate installs as a percentage of total installs
app_amount_by_category <- app_amount_by_category %>% mutate(perc = (installs/total_installs) * 100)
app_amount_by_category <- app_amount_by_category %>% mutate(perc = round(perc, 2))

top_5_app_amount_by_category <- app_amount_by_category %>% 
  mutate(Category = fct_lump_n(Category, 5, other_level="Other")) %>%
  arrange(desc(installs))

top_5_app_amount_by_category <- app_amount_by_category %>% 
  slice_max(order_by = installs, n = 5) %>%
  mutate(Category = ifelse(row_number() <= 5, as.character(Category), "Other"))

print(top_5_app_amount_by_category)
  



# Display as a pie chart
ggplot(top_5_app_amount_by_category, aes(x="", y=perc, fill=Category)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  
  theme_void() # remove background, grid, numeric labels
