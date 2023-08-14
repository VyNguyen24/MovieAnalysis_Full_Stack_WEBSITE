library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(stringr)
library(data.table)

### Source: https://www.geeksforgeeks.org/formatting-numbers-and-strings-in-r-programming-format-function/#
### Source: https://cran.r-project.org/web/packages/stringr/vignettes/stringr.html#:~:text=str_pad()%20pads%20a%20string,%2C%20right%2C%20or%20both%20sides.&text=(You%20can%20pad%20with%20other,by%20using%20the%20pad%20argument.)
### Source: https://www.geeksforgeeks.org/convert-column-classes-of-data-table-in-r/
### Source: https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
### Source: https://www.tutorialspoint.com/how-to-check-if-a-value-exists-in-an-r-data-frame-or-not
### Source: https://rstudio-pubs-static.s3.amazonaws.com/148979_34c44668be5045808581dda212e1c131.html
### Source: https://dplyr.tidyverse.org/reference/count.html#:~:text=count()%20lets%20you%20quickly,summarise(n%20%3D%20n())%20.
### Source: https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
### Source: https://rstudio.github.io/cheatsheets/strings.pdf
### Source: https://sparkbyexamples.com/r-programming/add-row-to-dataframe-in-r/#:~:text=To%20add%20a%20new%20row%20to%20the%20DataFrame%20(data.,the%20end%20of%20the%20dataframe.
### Source: https://stackoverflow.com/questions/24011246/deleting-rows-that-are-duplicated-in-one-column-based-on-the-conditions-of-anoth
### Source: ### https://r-graph-gallery.com/line-chart-ggplot2.html
### Source: https://canvas.uw.edu/courses/1643812/files/folder/Lectures_PG?preview=108264142
### Source: https://canvas.uw.edu/courses/1643812/files/folder/Lectures_PG?preview=108374725
### Source: https://shiny.posit.co/r/gallery/widgets/widget-gallery/
### Source: https://shiny.posit.co/r/reference/shiny/1.7.4/sliderinput
### Source: https://gallery.shinyapps.io/081-widgets-gallery/
### Source: https://stackoverflow.com/questions/24173595/r-shiny-using-a-slider-value-to-no-decimal-places
### Source: https://www.statology.org/r-get-column-names/#:~:text=To%20get%20the%20column%20names%20in%20a%20data%20frame%20in,What%20is%20this%3F&text=points%22%20%22team%22-,The%20result%20is%20a%20vector%20that%20contains%20all%20four%20column,frame%20listed%20in%20alphabetical%20order.
### Source: https://stackoverflow.com/questions/35040781/alignment-of-control-widgets-on-fluidpage-in-shiny-r
### Source: https://stackoverflow.com/questions/65472014/how-can-i-make-an-interactive-line-graph-through-shiny
### Source: https://stackoverflow.com/questions/60887910/how-to-include-all-the-observations-in-the-range-of-sliderinput-in-shiny-in-r


movies_data <- read.csv("Movies.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

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

### Took some A4 code for this
### Sources:
### https://www.geeksforgeeks.org/how-to-create-a-plot-using-ggplot2-with-multiple-lines-in-r/#
### https://environmentalcomputing.net/graphics/ggplot/ggplot-labels/
### https://stackoverflow.com/questions/40675778/center-plot-title-in-ggplot2
### https://stackoverflow.com/questions/14794599/how-to-change-line-width-in-ggplot
### https://r-graph-gallery.com/line-chart-ggplot2.html
### https://www.statology.org/ggplot-line-type/
### https://www.r-bloggers.com/2022/07/how-to-change-background-color-in-ggplot2-3/ (reference)
### https://community.rstudio.com/t/adding-manual-legend-to-ggplot2/41651/3
### https://www.simonqueenborough.info/R/basic/lessons/Sequences_of_Numbers.html#:~:text=The%20simplest%20way%20to%20create,to%20see%20how%20it%20works.&text=That%20gave%20us%20every%20integer,counting%20number%2C%20including%200).
### http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
### https://bookdown.org/yih_huynh/Guide-to-R-Book/graphing-with-different-datasets.html

colors <- c("January" = "purple", "February" = "green","March" = "red", 
            "April" = "lightskyblue", "May" = "orange", "June" = "black", 
            "July" = "chocolate4", "August" = "darkturquoise", "September" = "forestgreen", 
            "October" = "hotpink", "November" = "blue", "December" = "seashell4")

all_data_one_frame <- data.frame(January = January_final$n, February = February_final$n, 
March = March_final$n, April = April_final$n, May = May_final$n, June = June_final$n, 
July = July_final$n,August = August_final$n, September = September_final$n, 
October = October_final$n, November = November_final$n, December = December_final$n)

all_data_with_year <- all_data_one_frame %>% mutate(year = January_final$release_year)

ui <- fluidPage(
  titlePanel("Movie Production by Month over Time"),
      fluidRow(column(width = 6, sliderInput("slider2", label = 
            h3("Film Production Years"), min = all_data_with_year$year[1], 
              max = all_data_with_year$year[54], value = c(all_data_with_year$year[1], 
                  all_data_with_year$year[54]), step = 1, sep = ""), plotOutput("years")
        ),
        column(width = 6, selectInput("select", label = h3("Select Month"), 
            choices = colnames(all_data_one_frame)), plotOutput("month")
        )
    )
)

server <- function(input, output) {
  choose_year <- ""
  choose_month <- ""
  output$years <- renderPlotly({
      choose_year <- reactive({all_data_with_year %>% 
          filter(year == input$slider2[1]:input$slider2[2])})
  })
  output$month <- renderPlotly({
    choose_month <- reactive({all_data_with_year %>% 
        select(input$select)})
  }) 
  movies_by_year_plot <- ggplot(choose_month)
}

movies_by_year_plot <- ggplot(all_data_with_year) +
  geom_line(aes(x = year, y = January,color = "January"), size = 1, linetype = 1) + 
  geom_line(aes(x = year, y = February, color = "February"), size = 1, linetype = 1) + 
  geom_line(aes(x = year, y = March, color = "March"), size = 1, linetype = 1) + 
  geom_line(aes(x = year, y = April, color = "April"), size = 1, linetype = 1) + 
  geom_line(aes(x = year, y = May, color = "May"), size = 1, linetype = 1) + 
  geom_line(aes(x = year, y = June, color = "June"), size = 1, linetype = 1) +   
  geom_line(aes(x = year, y = July, color = "July"), size = 1, linetype = 1) + 
  geom_line(aes(x = year, y = August, color = "August"), size = 1, linetype = 1) + 
  geom_line(aes(x = year, y = September, color = "September"), size = 1, linetype = 1) + 
  geom_line(aes(x = year, y = October, color = "October"), size = 1, linetype = 1) + 
  geom_line(aes(x = year, y = November, color = "November"), size = 1, linetype = 1) + 
  geom_line(aes(x = year, y = December, color = "December"), size = 1, linetype = 1) + 
  labs(y = "Number of Movies", x = "Year (1970-2023)", 
       title = "Number of Movies by Month vs Time", color = "Legend") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_color_manual(values = colors)

ggplotly(movies_by_year_plot)
shinyApp(ui = ui, server = server)