library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(scales)
movie_data <- read.csv("Desktop/movie.csv")

ui <- fluidPage(
  titlePanel("Top 100 Movies by World Sales"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("distributor_filter", "Filter by Distributor:", 
                  choices = c("All", unique(movie_data$Distributor))),
      textInput("movie_title_input", "Enter Movie Title:"),
      sliderInput("world_sales_slider", "World Sales Range:",
                  min = min(movie_data$World.Sales..in...),
                  max = max(movie_data$World.Sales..in...),
                  value = c(min(movie_data$World.Sales..in...), max(movie_data$World.Sales..in...)),
                  step = 1),
      uiOutput("selected_movie_info")
    ),
    mainPanel(
      plotlyOutput("top_movies_scatter_plot"),
      verbatimTextOutput("movie_info_output")
    )
  )
)
server <- function(input, output) {
  output$top_movies_scatter_plot <- renderPlotly({
    filtered_movies <- movie_data
    if (!is.null(input$distributor_filter) && input$distributor_filter != "All") {
      filtered_movies <- filtered_movies %>%
        filter(Distributor == input$distributor_filter)
    }
    if (!is.null(input$movie_title_input) && input$movie_title_input != "") {
      filtered_movies <- filtered_movies %>%
        filter(str_detect(Title, fixed(input$movie_title_input)))
    }
    filtered_movies <- filtered_movies %>%
      filter(World.Sales..in... >= input$world_sales_slider[1] &
               World.Sales..in... <= input$world_sales_slider[2])
    scatter_plot <- ggplot(filtered_movies, aes(x = reorder(Distributor, World.Sales..in...), y = World.Sales..in...)) +
      geom_point(aes(text = paste("Movie:", Title, "<br>World Sales:", scales::comma(World.Sales..in...))), 
                 color = "steelblue", size = 3) +
      labs(x = "Distributor", y = "World Sales", title = "Movies by World Sales") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(labels = scales::comma)
    
    ggplotly(scatter_plot, tooltip = "text")
  })
  output$movie_info_output <- renderPrint({
    if (!is.null(input$movie_title_input) && input$movie_title_input != "") {
      movie_info <- movie_data %>%
        filter(str_detect(Title, fixed(input$movie_title_input))) %>%
        select(Movie.Info)
      if (nrow(movie_info) > 0) {
        movie_info$Movie.Info
      } else {
        "Movie not found."
      }
    }
  })
  output$selected_movie_info <- renderUI({
    selected_distributor <- input$distributor_filter
    if (!is.null(selected_distributor) && selected_distributor != "All") {
      distributor_movies <- movie_data %>%
        filter(Distributor == selected_distributor)
      if (nrow(distributor_movies) > 0) {
        movie_list <- lapply(distributor_movies$Title, function(title) {
          tags$li(title)
        })
        tags$ul(movie_list)
      } else {
        "No movies found for the selected distributor."
      }
    } else {
      ""
    }
  })
}

shinyApp(ui, server)
