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
library(googleVis)


#Data Work
population_data <- read_csv("Tidy_brewery_pop_data.csv")
review_data <- read_csv("Tidy_processed_review.csv") %>% na.omit()
review_data$style = as.factor(review_data$style)
popchoices <- c("Number of Breweries", "Residents per Brewery")
yearchoices <- as.character(1984:2018)
statechoices <- unique(population_data$State)
reviewchoices <- c("Most Popular Style",
                   "Mean ABV",
                   "Mean Rating",
                   "Most Reviewed Brewery")

##UI
ui <- dashboardPage(
  skin = 'black',
  dashboardHeader(title = "Beer in America",
                  titleWidth = 300),
  
  #https://shiny.rstudio.com/reference/shiny/1.0.1/icon.html (choose icon)
  dashboardSidebar(
    sidebarUserPanel("Graeme Keleher"),
    sidebarMenu(
      menuItem("Info", tabName = "Info_page", icon = icon("beer")),
      menuItem("Number of Breweries Chart", tabName = "Line_Plot", icon = icon("stats", lib = "glyphicon")),
      menuItem("Number of Breweries Map", tabName = "Population_Map", icon = icon("map")),
      menuItem("Craft Beer Reviews Map", tabName = "Reviews_Map", icon = icon("map"))
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
      
      # Intro Tab
      tabItem(tabName = 'Info_page',
              fluidRow(column(
                width = 6,
                box(
                  title = strong('Purpose of This Dashboard'),
                  solidHeader = T,
                  width = NULL,
                  h4("The purpose of this dashboard is to help the viewer get a high level understanding of the US brewing industry both
over the past decades and today. Data had been pulled 
                     from several different sources inclding the US Alcohol and Tobbacco Tax and Trade Bureau, population estimate data
                     from the ST. Luous Fed and a data set of scraped beer review data from Kaggle.")
                )
              ),
              column(
                width = 6,
                align = 'center',
                box(
                  background = "black",
                  width = NULL,
                  img(src = 'ipa.jpg', width = "600", align = "center")
                )
              )
              ),
              fluidRow(column(
                width = 12,
                box(
                  title = strong('A Note on the Data'),
                  solidHeader = T,
                  width = NULL,
                  h4("Take care before drawing conclusions based on the scraped review data. This data set may be biased in unknown ways. For example, in many states the brewery with the most reviews is significantly smaller than the brewery with the highest production")
                  )
                  ))), 
      
      # First tab content
      tabItem(tabName = "Population_Map",
              fluidRow(
                column(
                  width = 9,
                  box(
                      solidHeader = FALSE,
                      background = "black",
                      width = NULL,
                      height = "100%",
                      htmlOutput("popmapPlot"))
                  
                  
                ),
                column(
                  width = 3,
                  box(width = NULL,
                    title = "Data Options",
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
                column(
                  width = 9,
                  box(
                    solidHeader = FALSE,
                    background = "black",
                    width = NULL,
                    height = "100%",
                    plotlyOutput("poplinePlot"))
                  
                  
                ),
                column(
                  width = 3,
                  box(width = NULL,
                      title = "Data Options",
                      background = "black",
                      
                      selectInput("pop_choice1", 
                                  "Choose Data:",
                                  choices = popchoices,
                                  selected = "Number of Breweries"),
                      
                      sliderInput("slider", label = h3("Year Range"), min = 1984, 
                                  max = 2018, value = c(1995, 2008)),
                      br(),
                      
                      selectInput("state_choice1", 
                                  "Choose State:",
                                  choices = statechoices,
                                  selected = "CT"),
                      selectInput("state_choice2", 
                                  "Choose State:",
                                  choices = statechoices,
                                  selected = "CT"),
                      selectInput("state_choice3", 
                                  "Choose State:",
                                  choices = statechoices,
                                  selected = "CT")
                      
                       
                  )
                )
                
                
              )
      ),
      
      # Reviews content
      tabItem(tabName = "Reviews_Map",
              fluidRow(
                column(
                  width = 9,
                  box(
                    solidHeader = FALSE,
                    background = "black",
                    width = NULL,
                    height = "100%",
                    htmlOutput("ReviewPlot"))
                  
                  
                ),
                column(
                  width = 3,
                  box(width = NULL,
                      title = "Data Options",
                      background = "black",
                      selectInput("Subject",
                                  "Choose Data Subject:",
                                  choices = reviewchoices,
                                  selected = "Mean ABV") 
                  )
                )
                
                
              )
      )
              
    )
  )
)

##Server
server <- function(input, output) {
  
  output$popmapPlot <- renderGvis({
    
    year_choosen = input$year_choice
    
    if(input$pop_choice == "Number of Breweries"){
      type_choosen = "number_breweries"
    }else{
      type_choosen = "residents_per_brewery"
    }
    
    filtered_pop <- population_data%>%
      filter(year == year_choosen)
    
    myoptions <- list(region="US",
                      displayMode="regions", 
                      resolution="provinces",
                      colorAxis = "{colors:['#FFF5D9','#FFBC00']}",
                      width="auto", height="auto")
    
    gvisGeoChart(filtered_pop, locationvar = "State", 
                        colorvar=type_choosen,
                        options= myoptions) 
    
    
    })
  
  
  
  
  
  output$poplinePlot <- renderPlotly({
    s = c(input$state_choice1,input$state_choice2,input$state_choice3)
    r = input$slider
    
    if(input$pop_choice1 == "Number of Breweries"){
      type_choosen = "number_breweries"
    }else{
      type_choosen = "residents_per_brewery"
    }
    
    p = population_data %>% 
      na.omit() %>%
      filter(State %in% s) %>% 
      filter(year > r[1] & year < r[2])%>%
      ggplot(aes(year,get(type_choosen),color = State)) + 
      geom_line(size = 2) +
      labs(title = input$pop_choice1, 
           x = "Year", 
           y = "Residents per Brewery") 
      
    
    ggplotly(p)
    
  })
  
  
  c("Most Popular Style",
    "Mean ABV",
    "Mean Rating",
    "Most Reviewed Brewery")
  
  output$ReviewPlot <- renderGvis({
  
    if(input$Subject == "Most Popular Style"){
      type_choosen = "style"
    }else if(input$Subject == "Mean ABV"){
      type_choosen = "mean_abv"
    }else if(input$Subject == "Mean Rating"){
      type_choosen = "mean_rating"
    }else{
      type_choosen = "brewery_name"
    }
    
    
    myoptions <- list(region="US",
                      displayMode="regions", 
                      resolution="provinces",
                      colorAxis = "{colors:['#FFF5D9','#FFBC00']}",
                      width="auto", height="auto")
    
    gvisGeoChart(review_data, locationvar = "state", 
                 colorvar=type_choosen,
                 options= myoptions) 
    
    
  })
  
}

shinyApp(ui, server)
