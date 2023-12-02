summary(google_playstore$Rating)

# Histogram of Ratings
ggplot(google_playstore, aes(x = Rating)) +
  geom_histogram(binwidth = 0.5) +
  labs(title = "Distribution of Ratings",
       x = "Rating",
       y = "Frequency") +
  theme_minimal()

google_playstore %>%
  ggplot(aes(x = Category, y = Rating)) +
  geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of Ratings by Category",
       x = "Category",
       y = "Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

