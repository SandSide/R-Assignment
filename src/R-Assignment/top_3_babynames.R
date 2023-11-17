#library(babynames)
#library(tidyverse)

top_3_babynames <- babynames %>% 
  filter(year >= 1900 & year <=1999, sex == 'M') %>% 
  group_by(name) %>%
  summarise(total = sum(n)) %>%
  arrange(desc(total)) %>%
  slice_head(n = 3) %>%
  select(name) 

babynames_of_intrest <- babynames %>%
  filter(name %in% top_3_babynames$name, year >= 1900 & year <=1999, sex == 'M') %>%
  select(name, year, n) %>%
  arrange(name)
  
ggplot(babynames_of_intrest, aes(x = year, y = n, group = name, color = name)) +
  geom_line() +
  geom_point(shape = 16, size = 1) +
  labs(title = "Line Plot of the Number of Babynames for Top 3 Babynames in US Over Years",
       x = "Year",
       y = "Number of Babies with Name") +
  theme_minimal()


  
