summary(google_playstore$Rating)

# Histogram of Ratings
rating_dist <- google_playstore %>%
  filter(Rating > 0) %>%
  ggplot(aes(x = Rating)) +
    geom_histogram(binwidth = 0.5, color = 4, fill = 'white') +
    labs(title = 'Distribution of Ratings',
       x = 'Rating',
       y = 'Frequency')

# History of ratings for each category
category_rating_dist <- google_playstore %>%
  filter(Rating > 0) %>%
  ggplot(aes(x = Rating)) +
  geom_histogram(binwidth = 0.5, color = 4, fill = 'white') +
  labs(title = 'Distribution of Ratings',
       x = 'Rating',
       y = 'Frequency') +
  facet_wrap(~Category, scales = "free")


# Distribution of ratings for casino
casino_rating_dist <- google_playstore %>%
  filter(Category == 'Casino',
         Rating > 0) %>%
  ggplot(aes(x = Rating)) +
  geom_histogram(binwidth = 0.5, color = 4, fill = 'white') +
  labs(title = 'Distribution of Ratings',
       x = 'Rating',
       y = 'Frequency')
  
# Distribution of ratings for shopping and events categories
shopping_events_rating_dist <- google_playstore %>%
  filter(Category == 'Shopping' | Category == 'Events',
         Rating > 0) %>%
  ggplot(aes(x = Rating)) +
  geom_histogram(binwidth = 0.5, color = 4, fill = 'white') +
  labs(title = 'Distribution of Ratings',
       x = 'Rating',
       y = 'Frequency') +
  facet_wrap(~Category, scales = "free")
