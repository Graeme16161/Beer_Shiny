library(plotly)


f = "number_breweries"

filtered_pop <- population_data%>%
  filter(year == 2017)

filtered_pop$hover <- with(filtered_pop, f)

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

p <- plot_geo(filtered_pop, locationmode = 'USA-states') %>%
  add_trace(
    z = ~get(f), text = ~hover, locations = ~State,
    color = ~get(f), colors = 'Blues'
  ) %>%
  colorbar(title = "Number of\nBreweries") %>%
  layout(
    title = '2018 US Breweries by State<br>(Hover for breakdown)',
    geo = g
  )
