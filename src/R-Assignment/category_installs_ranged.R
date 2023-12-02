range_categories <- function(){
  
  # Define install ranges
  install_ranges <- c(0, 100000, 1000000, 10000000, Inf)
  install_ranges_labels <- c('Low', 'Medium', 'High', 'Very High')
  
  # Group based on categories and install ranges in the category
  category_installs_ranged <- google_playstore %>%
    select(Category, Maximum.Installs) %>%
    mutate(install_range = cut(Maximum.Installs, breaks = install_ranges, labels = install_ranges_labels, include.lowest = TRUE)) %>%
    group_by(Category, install_range) %>%
    summarise(total_installs = sum(Maximum.Installs), avg_installs = mean(Maximum.Installs), median_installs = median(Maximum.Installs))
  
  return(category_installs_ranged)
}

calculate_difference_from_range <- function(){
  
  low_installs <- category_installs_ranged %>% 
    filter(install_range == 'Low') %>%
    mutate(range_diff = (median_installs/100000) * 100)
  
  med_installs <- category_installs_ranged %>% 
    filter(install_range == 'Medium') %>%
    mutate(range_diff = (median_installs - 100000)/100000 * 100)
  
  high_installs <- category_installs_ranged %>% 
    filter(install_range == 'High') %>%
    mutate(range_diff = (median_installs - 1000000)/1000000 * 100)
  
  very_high_installs <- category_installs_ranged %>% 
    filter(install_range == 'Very High') %>%
    mutate(range_diff = (median_installs - 10000000)/10000000 * 100)
  
  
  dfs <- list(low_installs, med_installs, high_installs, very_high_installs)
  category_installs_ranged <- bind_rows(dfs)
  
  return(category_installs_ranged)
}

category_installs_ranged <- range_categories()
category_installs_ranged <- calculate_difference_from_range()


# Display X range of installed apps
category_installs_ranged %>% 
  filter(install_range == 'Very High') %>%
  ggplot(aes(x = median_installs , y = reorder(Category, median_installs ), fill = Category)) +
  geom_bar(stat = 'identity', width = 1, color = 'white') +
  labs(title = 'Median number of installs per category in low install range', x = 'Median Number of Installs', y = 'Category', fill = 'Category') +
  theme_minimal() + 
  guides(fill = 'none')








category_installs_ranged %>% ggplot(aes(x = 1, y = range_diff, color = install_range, shape = install_range)) +
  geom_point(position = position_jitter(width = 1), size = 2) +
  labs(title = 'Difference between media install and install range as a percentage for each category', x = 'Install Range', y = 'Percentage Diffrence from Median') +
  theme_minimal() +
  theme(axis.title.x=element_blank(), axis.text.x = element_blank())


category_installs_ranged_summary <- category_installs_ranged %>%
  group_by(install_range) %>%
  summarise(avg_installs = mean(avg_installs), median_installs = median(median_installs))




