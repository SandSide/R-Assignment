summary(google_playstore$Rating)

# Histogram of Ratings
google_playstore %>%
  filter(Rating > 0) %>%
  ggplot(aes(x = Rating)) +
    geom_histogram(binwidth = 0.5, color = 4, fill = 'white') +
    labs(title = 'Distribution of Ratings',
       x = 'Rating',
       y = 'Frequency')

