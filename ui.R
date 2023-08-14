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

movies_data <- read.csv("Movies.csv", 
                        header = TRUE, sep = ",", stringsAsFactors = FALSE)

all_films_by_license <- movies_data %>% select(License, World.Sales..in...)

mean_money_film <- all_films_by_license %>% group_by(License) %>% 
  summarise(avg_revenue = mean(World.Sales..in...)) %>% 
  filter(License != "NA")

ui <- navbarPage("Creating The Optimal Movie", 
              
### 1st tab
                 
tabPanel("Introduction",  fluidPage(
  h1("Creating The Optimal Movie", align="center"),
  img("Star Wars: The Force Awakens; The highest-ranked film from our dataset", 
      src = "/Users/dhruv/Downloads/image_24641330.jpeg"),
  h2("Main Question:"),
  p("How do various factors impact the box office performance of movies?"),
  p("This research project aims to analyze the top 1000 highest-grossing movies, 
  investigating the relationships between distributor, runtime, genre, and 
  tags with sales from domestic to international."),
  h2("Why is this important?"),
  p("Our research project aims to analyze the top 1000 highest-grossing movies, 
    investigating the relationships between distributor, runtime, genre, and world sales."),
  p("Our research questions revolve around whether the runtime and genre have an impact on 
    sales, which distributor produces the highest number of films in the top 1000, 
    and whether releasing movies under various genres affects box office performance."), 
  p("Addressing these research questions is motivated by the desire to gain valuable insights 
    into the intricate dynamics of the film industry. Understanding the potential influence 
    of runtime and genre on the number of sales can offer invaluable information to filmmakers 
    and movie studios while gaining valuable information about viewers and measuring elasticity."),
  p("This knowledge can help them better comprehend audience preferences, 
    refine marketing strategies, and optimize film production to cater to viewers’ 
    tastes effectively.  By answering these questions, valuable insights can be gained into audience preferences, industry trends, and the strategies that lead to box office success. "),
  p("The findings will benefit filmmakers, distributors, and other stakeholders in the 
  film industry, providing them with data-driven guidance to create 
  more engaging and commercially successful films in the future. "), 
  p("This helps shed light on the major distributors in the film industry and uncover 
  their success in the film industry while still exploring the effect of releasing movies 
  under multiple genres on box office performance can provide insights into audience responsiveness to genre-blending and diverse storytelling. 
  This knowledge can foster new creation, and storytelling, and enhance the viewer experience."), 
  h2("What are our questions?"),
  p("- Does the the distributor affect the number of sales?"), 
  p("- Which distributor produces the highest number of films in the top 1000 movies?"),
  p("- Has the amount of movie releases across the months of the year changed 
  over the duration of this dataset?"),
  p("- Does the movie license offered by distributors impact the Movie Box office 
    performance?"),
  p("- Does the movie genre offered by distributors impact the Movie Box office 
    performance?"),
  h2("What data are we using for this?"),
  h3("Dataset"),
  p("The dataset used for this project came from Kaggle:"),
  a("Top 1000 Highest Grossing Movies", 
    href = "https://www.kaggle.com/datasets/sanjeetsinghnaik/top-1000-highest-grossing-movies"),
  h3("Origin of Dataset"),
  p("The data was updated 2 years ago by Sanjeet Singh Naik. This data was collected from 
  various websites and combined for use in a variety of data operations. 
  The information came from numerous websites, including IMDB, Rotten Tomatoes, and others."),
  p("The data is gathered for the public's use, particularly for those who are interested in 
    movies because it contains a lot of observations (movies) and information about genre, duration, and sales value."),
  p("Producers and directors can use it to research audience preferences and current movie 
  trends. Additionally, by collecting information on domestic and foreign sales, this data 
  is also made for business purposes by giving investors a more thorough picture of the 
  movie's revenue."),
  h3("Ethical Questions (Questions of Power):"),
  p("- Is the dataset biased in any way that might favor a particular genre?"),
  p("- Does the content of the movies reflect racism or sensitivity to a particular 
  culture or minority group?"),
  h3("Limitations"),
  p("Possible limitations and problems with this data is that there are some missing values 
    in this data, which could potentially lead to biased results because the data does 
    not fully represent the whole.  Furthermore, the dataset only includes movies from 
    the United States, so it cannot be used to analyze other countries or to examine 
    diversity."),
    )
  ),

  ### Stuff for 2nd tab

  tabPanel("Distributor vs Movie Production", fluidPage(
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
        ),
    p("To answer the research question 'which distributors produce the most movies' as well 
    as the productivity of movie production across distributors, I use a bar chart to 
    visualize this comparison."),
    p("As 'Distributors' is a categorical value, using bar chart will provide a 
      straightforward way to observe the frequency or count of each movie in the dataset. 
      Additionally, it displays the category labels clearly and arranges the value assigned 
      to each distributor in a tidy manner."),
    p("The X axis in this plot represents the overall number of films, and the Y axis 
      represents the various distributors. In comparison to other distributors, 
      Warner Bros. appears to produce the most movies.")
      )
    )
  ),

### Stuff for 3rd tab

  tabPanel("Distributor vs Movie World Sales", fluidPage(
    titlePanel("Top Movie Distributor by World Movie Sales"),
    
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
  ),

  tabPanel("Movie Rating vs Movie World Sales", fluidPage(
    h1("Worldwide Sales vs Movie Licenses"),
    mainPanel(
      plotlyOutput("plot"),
      p("I chose a bar graph to represent the relationship between movie license/rating (a categorical variable) and the average worldwide sales in dollars (a continuous variable)."),

      p("I did so because bars are the best way to show the quantitative differences between the movie license categories represented on the X-axis due to the space they encompass, making said differences in average worldwide sales more noticeable to the viewer's eye.

I think this chart is especially useful because it could provide useful information to a potential film director on which movie rating to design/cater to based on the highest mean influx of money worldwide (based on the top 1000 domestically highest-performing movies recorded).

While all of the movie licenses make the same income on the scale of hundreds of millions (according to this dataset), rated R movies from this dataset perform the lowest in terms of mean worldwide sales.

G-rated and PG-13-rated films from this dataset averaged almost identically with PG-rated films from this dataset averaging slightly less at dollars."),

      p("The main takeaway from this bar graph is that according to this dataset, a film director designing a movie script for any of these 4 ratings will make money on the same scale, but to optimize worldwide sales, they should significantly prioritize G & PG-13-rated films over R-rated films."),

      p("-   Source: Sheamin's video on Panopto (wouldn't knit if I included the link)"),
      )
    )
  ),

  tabPanel("Movie Production by Month over time"),

  tabPanel("Genre vs Movie World Sales", fluidPage(
    titlePanel("Genre vs Movie World Sales"),
    mainPanel(
      plotlyOutput("barchart"),
      p("Selecting the appropriate type of chart or graph is vital when visualizing data. It guarantees that the information is presented in a clear and understandable format, especially when dealing with categorical values like the different genres of the top 1000 highest-grossing movies."),
      p("A bar chart is often the most effective choice in this case as it provides a simple way to comprehend the data. The chart reveals that some movies belong to more than one genre, making it difficult to determine the most prevalent genre. Nevertheless, the chart highlights the genre that appears frequently in the top 1000 highest-earning movies, considering the revenue generated globally by each genre. This enables us to identify the frequently occurring genre while providing a comprehensive overview of the data.
        ")
      )
    )
  ),

  tabPanel("Summary and Overall Conclusions/Takeaways")
)