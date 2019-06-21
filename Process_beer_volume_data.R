library(tidyverse)


Beer_bottles_cans <- read_csv("Beer_bottles_cans.csv")
Beer_kegs <- read_csv("Beer_kegs.csv")
Beer_in_brewery <- read_csv("Beer_on_brewery_premis.csv")




tidy_bottle_can_production <- gather(Beer_bottles_cans,"year","bottle_can_production", -STATE)%>%
  mutate(year = as.numeric(year))%>%
  filter(!is.na(STATE))


tidy_keg_production <- gather(Beer_kegs,"year","keg_production", -STATE)%>%
  mutate(year = as.numeric(year))%>%
  filter(!is.na(STATE))

tidy_sold_in_brewery <- gather(Beer_in_brewery,"year","sold_in_brewery", -STATE)%>%
  mutate(year = as.numeric(year))%>%
  filter(!is.na(STATE))


beer_production_data <- full_join(tidy_bottle_can_production,tidy_keg_production, by =  c("STATE","year"))%>%
  full_join(tidy_sold_in_brewery, by =  c("STATE","year"))


beer_production_data[is.na(beer_production_data)] <- 0
  
beer_production_data <- beer_production_data%>%
  mutate(total_production = bottle_can_production + keg_production + sold_in_brewery)

tidy_pop_year_only <- read_csv("tidy_pop_year_only.csv", 
                               col_types = cols(X1 = col_skip()))


production_plus = full_join(beer_production_data,tidy_pop_year_only,by = c("STATE" = "State","year"))%>%
  na.omit() %>%
  mutate(percent_in_brewery = sold_in_brewery/total_production*100)%>%
  mutate(prodcution_PP = total_production/population)%>%
  mutate(consumed_in_brewery_PP = sold_in_brewery/population)






production_plus2 <- production_plus %>%
  mutate(percent_in_can = bottle_can_production/total_production*100)%>%
  mutate(percent_in_keg = keg_production/total_production*100)
  

write.csv(production_plus2,"beer_prodction_final.csv")


production_plus%>%
ggplot(aes(percent_in_brewery,prodcution_PP))+
  geom_point()
