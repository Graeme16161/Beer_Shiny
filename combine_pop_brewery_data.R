df_final = full_join(tidy_breweries,tidy_pop_year_only,by = c("State","year"))%>%
  na.omit() %>%
  mutate(residents_per_brewery = population/number_breweries)


s = c("CT","MS","NY","AL")

df_final %>% 
  na.omit() %>%
  filter(State %in% s) %>% 
  ggplot(aes(year,residents_per_brewery,color = State)) + 
  geom_line(size = 2) +
  labs(title = "State residents per Brewery (in 1000's)", 
       subtitle = "Per the Alcohol and Tabbacco Tax and Trade Bureau",
       x = "Year", 
       y = "Residents per Brewery")+
  theme(legend.position = c(.8,.8))