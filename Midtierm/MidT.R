library(shiny)
library(tidyverse)
library(ggrepel)
library(here)

alcohol_df <- read_csv(here("data/alcohol.csv"))
ggplot(alcohol_df, aes(x = beer_servings, y = wine_servings)) +
  geom_point() +
  geom_label_repel(data = onecountry_df, aes(label = country)) +
  geom_point(data = onecountry_df, size = 3, shape = 1)

alc_choices <- c("beer_servings", "spirit_servings", "wine_servings")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("countryselect", label = "Choose a country for the scatterplot", choices = alcohol_df$country), 
      selectInput("alcvariable", label = "Choose a variable for the histogram", choices = alc_choices)
    ), 
    mainPanel(plotOutput("scatterplot"), 
              plotOutput("histogram"))
  )
)
server <- function(input, output, session) {
  
  onecountry_df <- reactive({
    alcohol_df %>% filter(country == input$countryselect)
  })
  
  output$scatterplot <- renderPlot(
    ggplot(data = alcohol_df, aes(x = beer_servings, y = wine_servings)) +
      geom_point() +
      geom_label_repel(data = onecountry_df(), aes(label = country)) +
      geom_point(data = onecountry_df(), size = 3, shape = 1)
  )
  
  output$histogram <- renderPlot(
    ggplot(data = alcohol_df, aes(x = .data[[input$alcvariable]])) + geom_histogram()
  )
}
shinyApp(ui, server)