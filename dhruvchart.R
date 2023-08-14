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

January <- filter_by_month("January")

February <- filter_by_month("February")

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

movies_by_year_plot <- ggplot() +
geom_line(data = January_final, aes(x = January_final$release_year, y = January_final$n,
color = "January"), size = 1, linetype = 1) + 
geom_line(data = February_final, aes(x = February_final$release_year, y = February_final$n, 
color = "February"), size = 1, linetype = 1) + 
geom_line(data = March_final, aes(x = March_final$release_year, y = March_final$n, 
color = "March"), size = 1, linetype = 1) + 
geom_line(data = April_final, aes(x = April_final$release_year, y = April_final$n, 
color = "April"), size = 1, linetype = 1) + 
geom_line(data = May_final, aes(x = May_final$release_year, y = May_final$n,
color = "May"), size = 1, linetype = 1) + 
geom_line(data = June_final, aes(x = June_final$release_year, y = June_final$n,
color = "June"), size = 1, linetype = 1) +   
geom_line(data = July_final, aes(x = July_final$release_year, y = July_final$n,
color = "July"), size = 1, linetype = 1) + 
geom_line(data = August_final, aes(x = August_final$release_year, y = August_final$n,
color = "August"), size = 1, linetype = 1) + 
geom_line(data = September_final, aes(x = September_final$release_year, y = September_final$n,
color = "September"), size = 1, linetype = 1) + 
geom_line(data = October_final, aes(x = October_final$release_year, y = October_final$n,
color = "October"), size = 1, linetype = 1) + 
geom_line(data = November_final, aes(x = November_final$release_year, y = November_final$n,
color = "November"), size = 1, linetype = 1) + 
geom_line(data = December_final, aes(x = December_final$release_year, y = December_final$n,
color = "December"), size = 1, linetype = 1) + 
labs(y = "Number of Movies", x = "Year (1970-2023)", 
title = "Number of Movies by Month vs Time", color = "Legend") +
theme(plot.title = element_text(hjust = 0.5)) + scale_color_manual(values = colors)

server <- function(input, output){
  
}

ui <- fluidPage()
