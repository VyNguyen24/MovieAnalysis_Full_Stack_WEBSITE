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
library(tidyr)
library(scales)
library(stringr)
library(data.table)

data <- read_csv("Movies.csv")
distributor_data <- data %>%
  group_by(Distributor) %>%
  mutate(num_movies = n())

movie_data <- read.csv("Movies.csv")

movies_data <- read.csv("Movies.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

all_films_by_license <- movies_data %>% select(License, World.Sales..in...)

mean_money_film <- all_films_by_license %>% group_by(License) %>% 
  summarise(avg_revenue = mean(World.Sales..in...)) %>% 
  filter(License != "NA")

server <- function(input, output){
  
  ### Stuff for 2nd tab
  
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
  
  
  ### Stuff for 3rd tab
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
 
  ### Stuff for Genre vs Sales chart
  genre_sales <- movie_data %>%
    separate_rows(Genre, sep = ", ") %>%  
    group_by(Genre) %>% 
    summarise(Total_Sales = sum(World.Sales..in...)) %>% 
    arrange(desc(Total_Sales)) %>%
    mutate(Genre = str_replace_all(Genre, "['\\[\\]]", ""))
  
  bar_chart <- ggplot(genre_sales, aes(x = reorder(Genre, Total_Sales), y = Total_Sales)) +
    geom_col(fill = "steelblue") +
    labs(x = "Genre", y = "Total Worldwide Sales", title = "Most Popular Movie Genres by Sales") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::comma)
  
  output$barchart <- renderPlotly({
    ggplotly(bar_chart)
  })
 
  ### Stuff for License vs Sales chart
  dhruv_bar_plot <- ggplot(mean_money_film, aes(x = License, y = avg_revenue)) +
    geom_bar(stat = "identity") + labs(y = "Average Worldwide Sales (Dollars)", 
                                       x = "Movie License (Rating)", title = "Worldwide Sales vs Movie Licenses", 
                                       color = "Legend") + theme(plot.title = element_text(hjust = 0.5))
  
  output$plot <- renderPlotly({
    ggplotly(dhruv_bar_plot)
  })
}