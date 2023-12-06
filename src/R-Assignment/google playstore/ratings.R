# This script contains code which details with analyzing trends in the rating column.

# Analyzing Ratings ----

# Get a summary of ratings
summary(google_playstore$Rating)

# Summaries ratings
rating_summary <- google_playstore %>%
  filter(Rating > 0) %>%
  summarise(mean = mean(Rating),
            median = median(Rating))

# Number of apps with rating of 0 
rating_0_apps <- google_playstore %>%
  filter(Rating == 0) %>%
  summarise(amount = n(),
            perc_of_apps = (n()/total_apps) * 100)

# Graphs ----

# Figure R1
# distribution of ratings of Ratings
rating_dist <- google_playstore %>%
  filter(Rating > 0) %>%
  ggplot(aes(x = Rating)) +
    geom_histogram(binwidth = 0.5, color = 4, fill = 'white') +
    labs(title = 'Distribution of Ratings',
       x = 'Rating',
       y = 'Frequency')


# Histogram of Ratings based on being free or not
free_rating_dist <- google_playstore %>%
  filter(Rating > 0) %>%
  ggplot(aes(x = Rating,  fill = Free)) +
  geom_histogram(binwidth = 0.5, color = 4) +
  labs(title = 'Distribution of Ratings',
       x = 'Rating',
       y = 'Frequency')


# Distribution of ratings for each category
category_rating_dist <- google_playstore %>%
  filter(Rating > 0) %>%
  ggplot(aes(x = Rating)) +
  geom_histogram(binwidth = 0.5, color = 4, fill = 'white') +
  labs(title = 'Distribution of Ratings by Category',
       x = 'Rating',
       y = 'Frequency') +
  facet_wrap(~Category, scales = "free")

# Figure R2
# History of ratings for multiple install ranges
install_ranges_rating_dist <- google_playstore %>%
  mutate(install_num = installs_to_num(Installs)) %>%
  filter(Rating > 0,
         install_num >= 1000,
         install_num <= 100000000) %>%
  group_by(Installs) %>%
  ggplot(aes(x = Rating)) +
  geom_histogram(binwidth = 0.5, color = 4, fill = 'white') +
  labs(title = 'Distribution of Ratings for Install 10,000+ to 100,000,000+',
       x = 'Rating',
       y = 'Frequency') +
  facet_wrap(~install_num, scales = 'free', labeller = labeller(install_num = num_to_installs), ncol = 3)


# Outlier Graphs ----

# Figure R3
# Distribution of ratings for events and shopping category
anomaly_category_rating_dist <- google_playstore %>%
  filter(Category == 'Events' | Category == 'Shopping',
         Rating > 0) %>%
  group_by(Category) %>%
  ggplot(aes(x = Rating)) +
  geom_histogram(binwidth = 0.5, color = 4, fill = 'white') +
  labs(title = 'Distribution of Ratings for Events and Shopping Category',
       x = 'Rating',
       y = 'Frequency') +
  facet_wrap(~Category, scales = 'free')

# Figure R4
# Distribution of ratings for casino category
casino_rating_dist <- google_playstore %>%
  filter(Category == 'Casino',
         Rating > 0) %>%
  ggplot(aes(x = Rating)) +
  geom_histogram(binwidth = 0.5, color = 4, fill = 'white') +
  labs(title = 'Distribution of Ratings for Casino Category',
       x = 'Rating',
       y = 'Frequency')
  


