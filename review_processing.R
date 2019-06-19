review_data <- read_csv("Tidy_review_data.csv")

#most popular style
popular_beers <- review_data %>%
  group_by(state)%>%
  count(style) %>% 
  top_n(1)

#average ABV

abv_beers <- review_data %>%
  group_by(state)%>%
  summarise(mean_abv = mean(abv, na.rm = TRUE))

#avergae rating
rating_beers <- review_data %>%
  group_by(state)%>%
  summarise(mean_rating = mean(average_rating, na.rm = TRUE))



#most reviewed brewery by state
top_state_brewery <- review_data %>%
  group_by(state,brewery_name)%>%
  summarise(brewery_total_revew = sum(number_review))%>%
  top_n(1)

b1 = inner_join(popular_beers,abv_beers, by = "state")%>%
  inner_join(rating_beers, by ="state")%>%
  inner_join(top_state_brewery, by = "state")
