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
library(scales)


#Datasets
beer_production_data <- read_csv("beer_prodction_final2.csv", 
                                 col_types = cols(X1 = col_skip()))
population_data <- read_csv("Tidy_brewery_pop_data.csv")
review_data <- read_csv("Tidy_processed_review.csv") %>% na.omit()
review_data$style = as.factor(review_data$style)


#choices vectors
popchoices <- c("Number of Breweries", "Residents per Brewery")
yearchoices <- as.character(1984:2018)
yearchoices2 <- as.character(2007:2018)
statechoices <- sort(unique(population_data$State))
statechoices <- append(statechoices,"Blank")
reviewchoices <- c("Mean ABV",
                   "Mean Rating")

production_choices <- c("Total Beer Production",
                        "Beer Consumed in Breweries (per person)",
                        "Beer Produced (per person)",
                        "Production consumed in Breweries (%)",
                        "Production sold in Bottles and Cans (%)",
                        "Production sold in Kegs (%)")

##UI
ui <- dashboardPage(
  skin = 'black',
  dashboardHeader(title = "Beer in America",
                  titleWidth = 300),
  
  #https://shiny.rstudio.com/reference/shiny/1.0.1/icon.html (choose icon)
  dashboardSidebar(
    sidebarUserPanel("Graeme Keleher"),
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "Info_page", icon = icon("beer")),
      menuItem("Number of Breweries History", tabName = "Line_Plot", icon = icon("chart-line")),
      menuItem("Number of Breweries Map", tabName = "Population_Map", icon = icon("map")),
      menuItem("Beer Production History", tabName = "Production_Plot", icon = icon("chart-line")),
      menuItem("Beer Production Map", tabName = "Production_Map", icon = icon("map")),
      menuItem("Craft Beer Attributes Map", tabName = "Reviews_Map", icon = icon("map"))
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
                  HTML(paste(h4("The purpose of this dashboard is to help the user understand the state of the American brewing industry through visualizations. Most of the data used in this dashboard comes from United Dtates Alcohol and Tobacco Tax and Trade Bureau which is a bureau within the Department of the Treasury. State population data, used to calculate \u0027per resident\u0027 statistics comes from the Federal Reserve Bank of St. Louis. Additionally, the \u0027Mean ABV\u0027 and \u0027Mean Rating\u0027 data comes from a Kaggle.com data set that was originally scraped from BeerAdvocate. Where as the other data comes from highly reputable sources, conclusions based on this data should be taken with a grain of salt. There may be unknown biases, both innocent and nefarious, within the data.  "),
                             '<br/>',
                             h4("It is important to understand how drastically the presence of large brewing facilities affects some of the production metrics. For example, in 2018 over 90% of beer brewed in Oklahoma was consumed on brewery premises. Whereas less than 1% of corresponding beer was consumed this way in neighboring Texas. This is undoubtably due to the presence of large Anheuser-Busch production facilities in the state which are far less likely to have on location tap rooms than small craft breweries."),
                             '<br/>',
                             h4("I especially enjoyed seeing the explosion of craft breweries reflected in the data. While the Alcohol and Tobacco Tax and Trade Bureau does not collect this data explicitly, various metrics such as \u0027barrels of beer consumed on a brewery premise\u0027 serve as good proxies."))
                        
                )
              )),
              column(
                width = 6,
                align = 'center',
                box(
                  background = "black",
                  width = NULL,
                  img(src = 'beer.jpg', width = "100%", align = "center")
                )
              )
              )), 
      
      ## Brewery Map
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
                                "Choose Brewery Data:",
                                choices = popchoices,
                                selected = "Number of Breweries") 
                  ),
                  box(width = NULL,
                      title = "Data Information",
                      background = "black",
                      textOutput("brewery_text")
                  )
                )

                
              )
      ),
      
      ## Beer production map
      tabItem(tabName = "Production_Map",
              fluidRow(
                column(
                  width = 9,
                  box(
                    solidHeader = FALSE,
                    background = "black",
                    width = NULL,
                    height = "100%",
                    htmlOutput("prodcutionmapPlot"))
                  
                  
                ),
                column(
                  width = 3,
                  box(width = NULL,
                      title = "Data Options",
                      background = "black",
                      selectInput("year_choice2",
                                  "Choose Year:",
                                  choices = yearchoices2,
                                  selected = "2018"),
                      
                      br(),
                      
                      selectInput("production_choices", 
                                  "Choose Production Data:",
                                  choices = production_choices,
                                  selected = "Total Beer Production") 
                  ),
                  box(width = NULL,
                      title = "Data Information",
                      background = "black",
                      textOutput("production_text")
                  )
                )
                
                
              )
      ),
      # BRewery line plots
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
                      
                      sliderInput("slider", label = h3("Year Range"), 
                                  min = 1984, 
                                  max = 2018,
                                  value = c(1995, 2018),
                                  sep = ""),
                      br(),
                      
                      selectInput("state_choice1", 
                                  "Choose State:",
                                  choices = statechoices,
                                  selected = "CT"),
                      selectInput("state_choice2", 
                                  "Choose State:",
                                  choices = statechoices,
                                  selected = "Blank"),
                      selectInput("state_choice3", 
                                  "Choose State:",
                                  choices = statechoices,
                                  selected = "Blank")
                      
                       
                  )
                )
                
                
              )
      ),
      # Beer Production line plots
      tabItem(tabName = "Production_Plot",
              fluidRow(
                column(
                  width = 9,
                  box(
                    solidHeader = FALSE,
                    background = "black",
                    width = NULL,
                    height = "100%",
                    plotlyOutput("production_Plot"))
                  
                  
                ),
                column(
                  width = 3,
                  box(width = NULL,
                      title = "Data Options",
                      background = "black",
                      
                      selectInput("production_choice1", 
                                  "Choose Data:",
                                  choices = production_choices,
                                  selected = "Total Beer Production"),
                      
                      br(),
                      
                      selectInput("state_choice1_2", 
                                  "Choose State:",
                                  choices = statechoices,
                                  selected = "CT"),
                      selectInput("state_choice2_2", 
                                  "Choose State:",
                                  choices = statechoices,
                                  selected = "Blank"),
                      selectInput("state_choice3_2", 
                                  "Choose State:",
                                  choices = statechoices,
                                  selected = "Blank")
                      
                      
                  )
                )
                
                
              )
      ),
      
      ## Reviews map
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
  
  output$brewery_text <-renderText({
    if(input$pop_choice == "Number of Breweries"){
      "Number of breweries measures the total number of breweries as licensed by the US Alcohol and Tobacco Tax and Trade Bureau." 
    }else{
      "Residents per brewery measures the number of state residents (in 1000s) per brewery located in the state. Note that the a darker color indicates MORE residents per brewery."   
    }
  })
  
  ###productionmap
  output$prodcutionmapPlot <- renderGvis({
    
    year_choosen = input$year_choice2
    
    if(input$production_choices == "Total Beer Production"){
      type_choosen = "total_production"
      pretty_var = "Total Production"
    }else if(input$production_choices == "Beer Consumed in Breweries (per person)"){
      type_choosen = "consumed_in_brewery_PP"
      pretty_var = "Brewery Consumption (PP)"
    }else if(input$production_choices == "Beer Produced (per person)"){
      type_choosen = "prodcution_PP"
      pretty_var = "Total Production (PP)"
    }else if(input$production_choices == "Production consumed in Breweries (%)"){
      type_choosen =  "percent_in_brewery"
      pretty_var = "Consumed in Brewery (%)"
    }else if(input$production_choices == "Production sold in Bottles and Cans (%)"){
      type_choosen =  "percent_in_can"
      pretty_var = "Canned or Bottled (%)"
    }else{
      type_choosen =  "percent_in_keg"
      pretty_var = "Kegged (%)"
    }
    
    filtered_production <- beer_production_data%>%
      filter(year == year_choosen)
    
    myoptions <- list(region="US",
                      displayMode="regions", 
                      resolution="provinces",
                      colorAxis = "{colors:['#FFF5D9','#FFBC00']}",
                      width="auto", height="auto")
    
    gvisGeoChart(filtered_production, locationvar = "STATE", 
                 colorvar=pretty_var,
                 options= myoptions) 
    
    
  })
  

  
  output$production_text <-renderText({
    if(input$production_choices == "Total Beer Production"){
      "Total beer production measures the beer production (in barrels) that is deemed taxable by the US Alcohol and Tobacco Tax and Trade Bureau. It should be noted that this statistic does not include exported beer, thus only beer destined for the American market is included in the map."
    }else if(input$production_choices == "Beer Consumed in Breweries (per person)"){
      "Beer Consumed in Breweries (per person) measures the amount of beer (in barrels) consumed on the premise of a brewery for each resident of the respective state. It should be noted that this statistic does not distinguish between state residents and visitors."
    }else if(input$production_choices == "Beer Produced (per person)"){
      "Beer Produced (per person) measures the amount of beer produced (in barrels) AND destined for the American market per resident of the respective state."
    }else if(input$production_choices == "Production consumed in Breweries (%)"){
      "Percentage of production consumed in breweries measures the percentage of a state\u0027s total beer production (destined for the American market) that is consumed on a brewery premise."
    }else if(input$production_choices == "Production sold in Bottles and Cans (%)"){
      "Percentage of production sold in bottles and cans measure the percentage of a state\u0027s total beer production (destined for the American market) that is sold in bottles and cans."
    }else{
      "Percentage of production sold in kegs measures the percentage of a state\u0027s total beer production (destined for the American market) that is sold in kegs."
    }
  })

  
  output$poplinePlot <- renderPlotly({
    s = c(input$state_choice1,input$state_choice2,input$state_choice3)
    r = input$slider
    
    if(input$pop_choice1 == "Number of Breweries"){
      type_choosen = "number_breweries"
      y_axis_choice = "Number of Breweries"
    }else{
      type_choosen = "residents_per_brewery"
      y_axis_choice = "Residents per Brewery (in 1000s)"
    }
    
    p = population_data %>% 
      na.omit() %>%
      filter(State %in% s) %>% 
      filter(year > r[1] & year < r[2])%>%
      ggplot(aes(year,get(type_choosen),color = State, group = State)) + 
      geom_line(size = 2,aes(text=sprintf("State: %s<br>Value: %g<br>Year: %g", State, get(type_choosen),year))) +
      labs(title = input$pop_choice1, 
           x = "Year", 
           y = y_axis_choice)+ 
      scale_y_continuous(breaks= pretty_breaks()) 
      
    
    ggplotly(p, tooltip="text")
    
  })
  
  output$production_Plot <- renderPlotly({
    s = c(input$state_choice1_2,input$state_choice2_2,input$state_choice3_2)
    
    if(input$production_choice1 == "Total Beer Production"){
      type_choosen = "total_production"
      y_axis_choice = "Beer in Barrels"
    }else if(input$production_choice1 == "Beer Consumed in Breweries (per person)"){
      type_choosen ="consumed_in_brewery_PP"
      y_axis_choice = "Beer in Barrels"
    }else if(input$production_choice1 == "Beer Produced (per person)"){
      type_choosen = "prodcution_PP"
      y_axis_choice = "Beer in Barrels"
    }else if(input$production_choice1 == "Production consumed in Breweries (%)"){
      type_choosen = "percent_in_brewery"
      y_axis_choice = "Percent of Total Production"
    }else if(input$production_choice1 == "Production sold in Bottles and Cans (%)"){
      type_choosen ="percent_in_can"
      y_axis_choice = "Percent of Total Production"
    }else{
      type_choosen ="percent_in_keg"
      y_axis_choice = "Percent of Total Production"
    }
    
    p = beer_production_data %>% 
      na.omit() %>%
      filter(STATE %in% s) %>% 
      ggplot(aes(x = year,y = get(type_choosen),  color = STATE, group = STATE)) + 
      geom_line(size = 2,aes(text=sprintf("State: %s<br>Value: %g<br>Year: %g", STATE, get(type_choosen),year))) +
      labs(title = input$production_choice1, 
           x = "Year", 
           y = y_axis_choice) 
    
    
    ggplotly(p, tooltip="text")
    
  })
  

  
  output$ReviewPlot <- renderGvis({
  
    if(input$Subject == "Mean ABV"){
      type_choosen = "mean_abv"
    }else{
      type_choosen = "mean_rating"
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
