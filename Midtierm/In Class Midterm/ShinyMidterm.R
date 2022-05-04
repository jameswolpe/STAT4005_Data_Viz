library(shiny)
library(tidyverse)
library(ggrepel)
library(here)
alcohol_df <- read_csv(here("data/alcohol.csv"))

onecountry_df <- alcohol_df %>% filter(country == "Australia")

ggplot(alcohol_df, aes(x = beer_servings, y = wine_servings)) +
  geom_point() +
  geom_label_repel(data = onecountry_df, aes(label = country)) +
  geom_point(data = onecountry_df, size = 3, shape = 1)
library(shiny)

alc_choices<- c("beer_servings", "spirit_servings", "wine_servings")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(selectizeInput(inputId = "alcvariable",
                                label = "Choose a Variable",
                                choices = names(alcohol_df)[c(2, 3, 4)),
    selectizeInput(inputId = "countryselect",
                   label = "Choose a Country",
                   choices = levels(factor(alcohol_df$country))),
    mainPanel(plotOutput("scatplot"))
)
)
server <- function(input, output, session) {
  df_onecount <- reactive({
    alcohol_df %>% filter(country == input$countryselect),
    
    output$scatplot <- renderPlot({
      ggplot(data = df_onecount(), aes(x = .data[[input$alcvariable]])) +
        geom_point(colour = "black", fill = "white")
  })
  
  
}

shinyApp(ui, server)