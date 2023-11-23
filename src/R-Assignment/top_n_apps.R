library(tidyverse)


google_playstore <- read.csv('Google-Playstore.csv')

categories <- google_playstore %>% select(Category) %>% distinct(Category)

google_playstore %>% filter(Category == "Puzzle") %>% select(App.Name)


google_playstore %>% 
  group_by(Category) %>% 
  summarize(total_installs = sum(Maximum.Installs))


top_100_installed_apps <- google_playstore %>% 
                            arrange(desc(Maximum.Installs)) %>% 
                            slice_head(n = 100)
colnames(google_playstore)


temp = top_100_installed_apps %>% select(App.Name, Maximum.Installs)

# Display top 100
ggplot(temp, aes(x = App.Name)) +
  geom_bar()
  