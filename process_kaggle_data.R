#filter for only non-retired beers and those from the US

kaggle_beers <- read_csv("beer_review_data/beers.csv", 
                  col_types = cols(notes = col_skip())) %>%
  filter(country == "US") %>%
  filter(retired == FALSE) %>%
  select(-country, -retired)


#filter for US breweries with "Brewery" in description, eg nothing that is just a bar
kaggle_breweries <- read_csv("beer_review_data/breweries.csv", 
                      col_types = cols(notes = col_skip()))%>%
  filter(country == "US") %>%
  filter(grepl("Brewery",types))%>%
  select(-country, -types, -city, -state)


#this takes ~30 seconds, select only the beer ID and the overall reviewer rating
#summerize to get number of reviews, average rating and sd of rating for each beer ID
kaggle_reviews <- read_csv("beer_review_data/reviews.csv", 
                    col_types = cols(date = col_skip(), 
                                     feel = col_skip(), look = col_skip(), 
                                     score = col_skip(), smell = col_skip(), 
                                     taste = col_skip(), text = col_skip(), 
                                     username = col_skip()))%>%
  group_by(beer_id)%>%
  na.omit()%>%
  summarise(average_rating = mean(overall),
            number_review = n(),
            sd_reviews = sd(overall))


#Inner join beer and reviews info on the beer ID

Kaggle_beers_with_reviews = inner_join(kaggle_beers,kaggle_reviews, by = c("id" = "beer_id"))


#inner join new df with brewery data for final data frame
Kaggle_beers_with_reviews_breweries = inner_join(Kaggle_beers_with_reviews,kaggle_breweries, by = c("brewery_id" = "id"))%>%
  mutate(beer_name = name.x, brewery_name = name.y)%>%
  select(-name.x,-name.y)


state_abv = Kaggle_beers_with_reviews %>% group_by(state) %>% summarise(avg_alc = mean(abv,na.rm = TRUE))



