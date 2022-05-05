library(tidyverse)
library(lubridate)
weather_df <- read_csv("data/canton_ny_weather_data.txt")

weather_df <- weather_df %>% mutate(month = month(datetime), 
                                    year = as.factor(year(datetime)), 
                                    day = yday(datetime)) %>% 
  select(month, year, day, everything())


temp_select <- names(weather_df)[c(6:13)]

years_select <- c("2020", "2021", "2022")


library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("yearselect", label = "Select a year", choices = years_select), 
      radioButtons("temperature", label = "Select a statistic", choices = temp_select)
    ), 
    mainPanel(plotOutput("scatterplot")))
)

server <- function(input, output, session) {
  
  weather_reactive <- reactive({
    weather_df %>% filter(year == input$yearselect)
  })
  
  output$scatterplot <- renderPlot(
    ggplot(data = weather_reactive(), aes(x = day, y = .data[[input$temperature]])) + 
      geom_point()+
      geom_smooth()
  )
}

shinyApp(ui, server)
