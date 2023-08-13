#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(tidyverse)
library(patchwork)
library(janitor)
library(bslib)
library(shinythemes)
library(shinydashboard)


data <- read_csv("Movies.csv")
distributor_data <- data %>%
  group_by(Distributor) %>%
  mutate(num_movies = n())

# Setuo ui
ui <- fluidPage(
  titlePanel("Movie Distributors vs Number of Movies"),
  theme = bs_theme(version = 4, bootswatch = "minty"),
  fluidRow(
    column(width = 3, height = 4,
           checkboxGroupInput("option", "Choose License",
                              c("PG-13" = "PG-13",
                                "G" = "G",
                                "R" = "R"))),
    column(width = 5,
           sliderInput("number", "Number of Movies:",
                       min = 0, max = 158, c(0,158))),
    column(width = 3,
           selectInput("distributor_filter", "Select Distributor:",
                       choices = c("ALL", unique(distributor_data$Distributor))))
    ),
  fluidRow(
    column(width = 12, height = 33,
           plotlyOutput("distributor_plot")
   ) 
  )
)

# Setup server 
server <- function(input, output) {
  output$distributor_plot <- renderPlotly({
    filtered_data <- distributor_data
    if (!is.null(input$distributor_filter) && input$distributor_filter != "ALL") {
      filtered_data <- filtered_data %>%
        filter(Distributor == input$distributor_filter)
    }
    if (!is.null(input$option) && length(input$option) > 0) {
      filtered_data <- filtered_data %>%
        filter(License %in% input$option)
    }
    if (!is.null(input$number) && input$number[1] > 0) {
      filtered_data <- filtered_data %>%
        filter(num_movies >= input$number[1])
    }
    if (!is.null(input$number) && input$number[2] < 158) {
      filtered_data <- filtered_data %>%
        filter(num_movies <= input$number[2])
    }
    
    # plotting
    p <- ggplot(filtered_data, aes(y = Distributor, x = num_movies, fill = Distributor)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Distributors vs Number of Movies", x = "Number of Movies", y = "Distributors") +
      theme(legend.position = "none") +
      xlim(0, max(filtered_data$num_movies))
    
    # plotly
    ggplotly(p)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
