library(googleVis)

library(tidyverse)

states <- read_csv("Bootcamp/RShiny Project/50_us_states_all_data.csv", 
                                   col_names = FALSE)

states1 = states %>%
  select(X2, X3)


Tidy_brewery_pop_data <- read_csv("Bootcamp/RShiny Project/App_Files/Tidy_brewery_pop_data.csv")

breweries2015 = Tidy_brewery_pop_data %>%
  filter( year == 2015) %>%
  right_join(states1, by = c("State" = "X3")) %>%
  mutate(stateyear = paste(year, X2, sep=" "))

names(breweries2015)[names(breweries2015) == 'number_breweries'] <- 'Number of Breweries'

myoptions <- list(region="US",
     displayMode="regions", 
     resolution="provinces",
     width=600, height=400)

G1a <- gvisGeoChart(breweries2015, locationvar = "X2", 
                    colorvar='Number of Breweries',
                    hovervar = 'stateyear',
                    options= myoptions) 

plot(G1a)


hist(breweries2015$residents_per_brewery)
