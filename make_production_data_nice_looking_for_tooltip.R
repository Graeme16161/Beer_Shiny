beer_prodction_final <- read_csv("beer_prodction_final.csv")


t = production_plus2 %>%
  mutate('Total Production' = round(total_production))%>%
  mutate('Brewery Consumption (PP)' = round(consumed_in_brewery_PP,2))%>%
  mutate('Consumed in Brewery (%)' = round(percent_in_brewery,2))%>%
  mutate('Canned or Bottled (%)' = round(percent_in_can,2))%>%
  mutate('Kegged (%)' = round(percent_in_keg,2)) %>%
  mutate('Total Production (PP)' = round(prodcution_PP,2))
  

library(tidyverse)

write.csv(t, "beer_prodction_final2.csv")
