#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# load libraries 
library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)


#Data Work
population_data <- read_csv("Tidy_brewery_pop_data.csv")
review_data <- read_csv("Tidy_review_data.csv")
popchoices = c("Number of Breweries", "Residents per Brewery")
yearchoices = as.character(1984:2018)
statechoices = unique(population_data$State)

##UI
ui <- dashboardPage(
  skin = 'black',
  dashboardHeader(title = "Beer in America by State",
                  titleWidth = 300),
  
  #https://shiny.rstudio.com/reference/shiny/1.0.1/icon.html (choose icon)
  dashboardSidebar(
    sidebarUserPanel("Test User"),
    sidebarMenu(
      menuItem("Number of Breweries Chart", tabName = "Line_Plot", icon = icon("stats", lib = "glyphicon")),
      menuItem("Number of Breweries Map", tabName = "Population_Map", icon = icon("map"))
    )
  ),
  
  
  dashboardBody(
    #header font
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tags$style(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    
    tabItems(
      # First tab content
      tabItem(tabName = "Population_Map",
              fluidRow(
                column(
                  width = 9,
                  box(
                      solidHeader = FALSE,
                      background = "black",
                      width = NULL,
                      height = "90%",
                      plotlyOutput("popmapPlot",height = "100%"))
                  
                  
                ),
                column(
                  width = 3,
                  box(width = NULL,
                    title = "Controls",
                    background = "black",
                    selectInput("year_choice",
                                "Choose Year:",
                                choices = yearchoices,
                                selected = "2018"),
                    
                    br(),
                    
                    selectInput("pop_choice", 
                                "Choose Data:",
                                choices = popchoices,
                                selected = "Number of Breweries") 
                  )
                )

                
              )
      ),
      
      # Second tab content
      tabItem(tabName = "Line_Plot",
              fluidRow(
                box(plotlyOutput("poplinePlot")),
                
                box(
                  
                  selectInput("state_choice", 
                              "Choose State:",
                              choices = statechoices,
                              selected = "CT")
                )
              )
      )
    )
  )
)

##Server
server <- function(input, output) {
  
  output$popmapPlot <- renderPlotly({
    
    year_choosen = input$year_choice
    
    if(input$pop_choice == "Number of Breweries"){
      type_choosen = "number_breweries"
    }else{
      type_choosen = "residents_per_brewery"
    }
    
    filtered_pop <- population_data%>%
      filter(year == year_choosen)
    
    filtered_pop$hover <- with(filtered_pop, paste(State, '<br>', input$pop_choice, type_choosen))
    
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    plot_geo(filtered_pop, locationmode = 'USA-states') %>%
      add_trace(
        z = ~get(type_choosen), text = ~hover, locations = ~State,
        color = ~get(type_choosen), colors = 'Blues'
      ) %>%
      colorbar(title = "Number of\nBreweries") %>%
      layout(
        title = paste(year_choosen, 'US Breweries by State<br>(Hover for breakdown)'),
        geo = g
      )
    
    })
  
  
  
  
  
  output$poplinePlot <- renderPlotly({
    s = input$state_choice
    
    p = population_data %>% 
      na.omit() %>%
      filter(State %in% s) %>% 
      ggplot(aes(year,residents_per_brewery,color = State)) + 
      geom_line(size = 2) +
      labs(title = "State residents per Brewery (in 1000's)", 
           subtitle = "Per the Alcohol and Tabbacco Tax and Trade Bureau",
           x = "Year", 
           y = "Residents per Brewery")
    
    ggplotly(p)
    
  })
  
}

shinyApp(ui, server)
