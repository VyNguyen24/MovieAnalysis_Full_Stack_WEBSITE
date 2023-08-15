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

convert_runtime <- function(runtime_string) {
  hours <- as.numeric(sub(".*?(\\d+) hr.*", "\\1", runtime_string, ignore.case = TRUE))
  minutes <- as.numeric(sub(".*?(\\d+) min.*", "\\1", runtime_string, ignore.case = TRUE))
  total_minutes <- hours * 60 + minutes
  return(total_minutes)
}

movie_data <- movie_data %>%
  mutate(Total.Minutes = sapply(Movie.Runtime, convert_runtime))

movies_data <- read.csv("Movies.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

all_films_by_license <- movies_data %>% select(License, World.Sales..in...)

mean_money_film <- all_films_by_license %>% group_by(License) %>% 
  summarise(avg_revenue = mean(World.Sales..in...)) %>% 
  filter(License != "NA")

January_final <- creating_plottable_df(January) 

February_final <- creating_plottable_df(February)

March_final <- creating_plottable_df(March)

April_final <- creating_plottable_df(April)

May_final <- creating_plottable_df(May)

June_final <- creating_plottable_df(June)

July_final <- creating_plottable_df(July)

August_final <- creating_plottable_df(August)

September_final <- creating_plottable_df(September)

October_final <- creating_plottable_df(October)

November_final <- creating_plottable_df(November)

December_final <- creating_plottable_df(December)

all_data_one_frame <- data.frame(
  Month = rep(c("January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December"), each = nrow(January_final)),
  Year = rep(January_final$release_year, times = 12),
  Count = c(January_final$n, February_final$n, March_final$n, April_final$n, May_final$n,
            June_final$n, July_final$n, August_final$n, September_final$n,
            October_final$n, November_final$n, December_final$n)
)

filtered_data <- movies_data %>% select(Release.Date) %>% 
  filter(Release.Date != "NA")

adding_year <- filtered_data %>% mutate(release_year = str_sub(Release.Date, 
                                                               str_length(Release.Date) - 4, str_length(Release.Date)))

adding_year_table <- data.table(adding_year)

casting_year <- data.frame(adding_year_table[ , release_year := as.integer(release_year)])

adding_month <- casting_year %>% mutate(release_month = 
                                          str_sub(Release.Date, 1, str_length(Release.Date) - 8))

adding_month_table <- data.table(adding_month)

trimming_month <- data.frame(adding_month_table[, 
                                                release_month := str_trim(release_month, side = c("both"))])

filter_by_month <- function(month){
  trimming_month %>% filter(release_month == month) %>% 
    arrange(release_year) %>% group_by(release_year) %>% select(release_year) %>% count()
}

January <- filter_by_month("January")

February <- filter_by_month("February")

March <- filter_by_month("March")

April <- filter_by_month("April")

May <- filter_by_month("May")

June <- filter_by_month("June")

July <- filter_by_month("July")

August <- filter_by_month("August")

September <- filter_by_month("September")

October <- filter_by_month("October")

November <- filter_by_month("November")

December <- filter_by_month("December")

add_sequence_function <- function(dataset){
  year = 1970
  while (year <= 2023){
    if (any(dataset$release_year) == year){
    }
    else {
      dataset[nrow(dataset) + 1,] <- list(year, 0)
      year = year + 1
    }
  }
  dataset %>% arrange(release_year)
  return (dataset)
}

creating_plottable_df <- function(dataset){
  add_sequence_function(dataset) %>% arrange(release_year) %>% 
    group_by(release_year) %>% arrange(release_year, -n) %>% 
    filter(duplicated(release_year) == FALSE)
}

January_final <- creating_plottable_df(January) 

February_final <- creating_plottable_df(February)

March_final <- creating_plottable_df(March)

April_final <- creating_plottable_df(April)

May_final <- creating_plottable_df(May)

June_final <- creating_plottable_df(June)

July_final <- creating_plottable_df(July)

August_final <- creating_plottable_df(August)

September_final <- creating_plottable_df(September)

October_final <- creating_plottable_df(October)

November_final <- creating_plottable_df(November)

December_final <- creating_plottable_df(December)

all_data_one_frame <- data.frame(
  Month = rep(c("January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December"), each = nrow(January_final)),
  Year = rep(January_final$release_year, times = 12),
  Count = c(January_final$n, February_final$n, March_final$n, April_final$n, May_final$n,
            June_final$n, July_final$n, August_final$n, September_final$n,
            October_final$n, November_final$n, December_final$n)
)

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
 
  output$vy_plot <- renderPlotly({
    filtered_data <- distributor_data
    p <- ggplot(filtered_data, aes(y = Distributor, x = num_movies, fill = Distributor)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Distributors vs Number of Movies", x = "Number of Movies", y = "Distributors") +
      theme(legend.position = "none") +
      xlim(0, max(filtered_data$num_movies))
    ggplotly(p)
  }) 
  
  ### Stuff for 3rd tab
  
  filtered_movies <- reactive({
    movie_data %>%
      filter(World.Sales..in... >= input$world_sales_slider[1] &
               World.Sales..in... <= input$world_sales_slider[2])
  })
  
  output$world_sales_vs_runtime_scatter_plot <- renderPlotly({
    scatter_plot <- ggplot(filtered_movies(), aes(x = World.Sales..in..., y = Total.Minutes, text = Title)) +
      geom_point(show.legend = FALSE) +
      labs(x = "World Sales", y = "Movie Runtime", title = "Movies: World Sales vs. Movie Runtime") +
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma) +
      theme_bw()
    
    ggplotly(scatter_plot, tooltip = "text", mode = "markers")
  })
  
  output$movie_info_output <- renderPrint({
    if (nrow(filtered_movies()) > 0) {
      paste("Total movies:", nrow(filtered_movies()))
    } else {
      "No movies found."
    }
  })
  
  output$selected_movie_info <- renderUI({
    if (nrow(filtered_movies()) > 0) {
      movie_list <- lapply(filtered_movies()$Title, function(title) {
        tags$li(title)
      })
      tags$ul(movie_list)
    } else {
      "No movies found."
    }
  })
  
  output$selected_movie_info <- renderUI({
    if (nrow(filtered_movies()) > 0) {
      movie_list <- lapply(filtered_movies()$Title, function(title) {
        tags$li(title)
      })
      tags$ul(movie_list)
    } else {
      "No movies found."
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
  
  output$pagna_bar_chart <- renderPlotly({
    ggplotly(bar_chart)
  })
 
  ### Stuff for Years vs Number of Movies Plot
  
  choose_month <- reactive({
    all_data_one_frame %>% filter(Month == input$select)
  })
  
  output$month <- renderPlotly({
    ggplotly(
      ggplot(choose_month(), aes(x = Year, y = Count)) +
        geom_line() + 
        labs(y = "Number of Movies", x = "Year (1970-2023)", 
             title = "Number of Movies by Month vs Time") +
        theme(plot.title = element_text(hjust = 0.5))
    )
  }) 
  
  ### Stuff for License vs Sales chart
  dhruv_bar_plot <- ggplot(mean_money_film, aes(x = License, y = avg_revenue)) +
  geom_bar(stat = "identity") + labs(y = "Average Worldwide Sales (Dollars)", 
  x = "Movie License (Rating)", title = "Worldwide Sales vs Movie Licenses", 
  color = "Legend") + theme(plot.title = element_text(hjust = 0.5))
  
  output$plot <- renderPlotly({
    ggplotly(dhruv_bar_plot)
  })
  
  output$dhruv_plot <- renderPlotly({
    ggplotly(dhruv_bar_plot)
  })
  
}