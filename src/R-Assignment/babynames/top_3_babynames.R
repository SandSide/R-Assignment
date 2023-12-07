library(babynames)
library(tidyverse)

# Filter for males born between 1900 and 1999
males_1900_1999 <- babynames %>% 
  filter(year >= 1900 & year <=1999, sex == 'M')

# Find top 3 most popular male baby names
top_3_babynames <- males_1900_1999 %>% 
  group_by(name) %>%
  summarise(total = sum(n)) %>%
  arrange(desc(total)) %>%
  slice_head(n = 3) %>%
  select(name) 

# Select records for males with names in top 3 most popular male baby names
babynames_of_intrest <- males_1900_1999 %>%
  filter(name %in% top_3_babynames$name) %>%
  select(name, year, n) %>%
  arrange(name)

# 

# Plot a line graph where x-axis represents years, and y-axis represents number of babies born with that name in that year
ggplot(babynames_of_intrest, aes( x = year, y = n, group = name, color = name)) +
  geom_line(size = 1) +
  labs(title = "Popularity of Top 3 Male Baby Names in US between the Years 1900 and 1999",
       x = "Year",
       y = "Number of Babies Born") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, max(babynames_of_intrest$n), by = 20000)) +
  scale_x_continuous(breaks = seq(0, max(babynames_of_intrest$n), by = 20))
