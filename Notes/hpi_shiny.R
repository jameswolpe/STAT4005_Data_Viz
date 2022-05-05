

library(tidyverse)
hpi_df <- read_csv("data/hpi-tidy.csv")


library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(selectizeInput#(inputId = "countryselect",
                               # label = "Choose a Country",
                                #choices = hpi_df$Country),
                 selectizeInput(inputId = "varselect",
                                label = "Choose a Variable",
                                choices = names(hpi_df)[c(3, 5, 7)])),
    mainPanel(plotOutput("histplot"))
  )
)
server <- function(input, output, session) {
  
  df_region <- reactive({
    hpi_df %>% filter(Country == input$countryselect)
  })
  output$histplot <- renderPlot({
    ggplot(data = df_region(), aes(x = .data[[input$varselect]])) +
      geom_histogram(colour = "black", fill = "white", bins = 15) + xlim(c(0,85))
  })
}
shinyApp(ui, server)