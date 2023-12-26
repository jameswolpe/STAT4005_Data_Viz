#All work presented is my own, and I have followed all rules for collaboration.


library(tidyverse)
library(shiny)
library(here)
hpi_df <- read_csv(here("data/hpi-tidy.csv"))


ui <- fluidPage(
  sidebarLayout(
  sidebarPanel(selectizeInput(inputId = "regionselect",
                               label = "Choose a Region",
                               choices = hpi_df$Region),
                selectizeInput(inputId = "varselect",
                              label = "Choose a Variable",
                              choices = names(hpi_df)[c(3, 5, 7)])),
  mainPanel(plotOutput("histplot"))
)
)
server <- function(input, output, session) {
  
  df_region <- reactive({
    hpi_df %>% filter(Region == input$regionselect)
})
  output$histplot <- renderPlot({
    ggplot(data = df_region(), aes(x = .data[[input$varselect]])) +
      geom_histogram(colour = "black", fill = "white", bins = 20) + xlim(c(22,85))
  })
}
shinyApp(ui, server)

#For my app I chose to use the happy planet data set to look at the graph of 3 variables:
#HPI Rank, (the rank of the country’s Happy Planet Index)
#Life Expectancy (the average life expectancy of a citizen), 
#Happy Life Years (a combination of life expectancy and wellbeing), and sort them by region.
# The purpose of this app was to select a variable and compare the distributions by each region.
# In order to more successfully compare, I set all graphs to the same window size
# so that you can see the differences from region to region.
