Number_of_breweries_by_state <- read_csv("Number_of_breweries_by_state.csv")



tidy_breweries = gather(Number_of_breweries_by_state,"year","number_breweries", -State)%>%
  mutate(year = as.numeric(year))%>%
  mutate(number_breweries = as.numeric(number_breweries))

s = c("CT","MA","OK","CO")

tidy_breweries %>% 
  na.omit() %>%
  filter(State %in% s) %>% 
  ggplot(aes(year,number_breweries,color = State)) + 
  geom_line(size = 2) +
  labs(title = paste("Number of Breweries in",s,"by Year"), 
       subtitle = "Per the Alcohol and Tabbacco Tax and Trade Bureau",
       x = "Year", 
       y = "Number of Breweries")+
  theme(legend.position = c(.1,.8))
