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
### Source: https://sparkbyexamples.com/r-programming/r-select-all-columns-except-column/#:~:text=To%20select%20all%20columns%20except,function%20from%20the%20dplyr%20package.
### Source: https://stackoverflow.com/questions/58448118/warning-jsonlite-in-shiny-input-to-asjsonkeep-vec-names-true-is-a-named-vecto
### Source: https://www.digitalocean.com/community/tutorials/r-melt-and-cast-function
### Source: https://www.statology.org/plot-multiple-columns-in-r/
### Source: https://stackoverflow.com/questions/44615406/error-in-as-data-frame-default-cannot-coerce-class-creactiveexpr-reactive
### Source: https://uw.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=1411ba43-2dce-48c2-b3c7-b052016ca270
### Source: https://uw.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=3f7708b9-fa00-4182-aba4-b057017603fd

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

all_data_one_frame <- data.frame(
  Month = rep(c("January", "February", "March", "April", "May", "June",
                "July", "August", "September", "October", "November", "December"), each = nrow(January_final)),
  Year = rep(January_final$release_year, times = 12),
  Count = c(January_final$n, February_final$n, March_final$n, April_final$n, May_final$n,
            June_final$n, July_final$n, August_final$n, September_final$n,
            October_final$n, November_final$n, December_final$n)
)

ui <- fluidPage(
  titlePanel("Movie Production by Month over Time"),
  fluidRow(
    column(width = 6, selectInput("select", label = h3("Select Month"), 
                                  choices = unique(all_data_one_frame$Month)), 
           plotlyOutput("month")
    )
  )
)

server <- function(input, output) {
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
}

shinyApp(ui = ui, server = server)