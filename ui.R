ui <- fluidPage(
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
  p("- Does the run time affect the number of sales?"), 
  p("- Which distributor produces the highest number of films in the top 1000 movies?"),
  p("- Between which years did certain distributors peak in terms of sales and movies made?"),
  p("- Has the amount of movie releases across the months of the year changed 
  over the duration of this dataset?"),
  p("- How does the generic tag offered by distributors impact the Movie Box office 
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
  h3("Ethical Questions (Questions of Power:"),
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