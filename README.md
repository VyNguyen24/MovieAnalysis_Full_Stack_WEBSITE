# Creating the Optimal Movie 
## INFO 201 "Foundational Skills for Data Science" — Summer 2023

Authors: 

Dhruv Jagannath

Pagna Keo

Vy Nguyen

Hao Zhu

https://dhruvj20.shinyapps.io/final-deliverable-p03-dhruvj20/

# Introduction

Main Question:

### How do various factors impact the box office performance of movies?

## Why is this important?

Our research project aims to analyze the top 1000 highest-grossing movies, 
investigating the relationships between distributor, runtime, genre, and world sales.

Our research questions revolve around whether the runtime and genre have an impact on 
sales, which distributor produces the highest number of films in the top 1000, 
and whether releasing movies under various genres affects box office performance.

Addressing these research questions is motivated by the desire to gain valuable insights 
into the intricate dynamics of the film industry. Understanding the potential influence 
of runtime and genre on the number of sales can offer invaluable information to filmmakers 
and movie studios while gaining valuable information about viewers and measuring elasticity.

This knowledge can help them better comprehend audience preferences, 
refine marketing strategies, and optimize film production to cater to viewers’ 
tastes effectively.  By answering these questions, valuable insights can be gained into audience preferences, industry trends, and the strategies that lead to box office success.

The findings will benefit filmmakers, distributors, and other stakeholders in the 
film industry, providing them with data-driven guidance to create 
more engaging and commercially successful films in the future. 

This helps shed light on the major distributors in the film industry and uncover 
their success in the film industry while still exploring the effect of releasing movies 
under multiple genres on box office performance can provide insights into audience responsiveness to genre-blending and diverse storytelling. This knowledge can foster new creation, and storytelling, and enhance the viewer experience. 

## What are our questions?
- Does the the distributor affect the number of sales?"

- Which distributor produces the highest number of films in the top 1000 movies?"

- Has the amount of movie releases across the months of the year changed 
  over the duration of this dataset?"
  
- Does the movie license offered by distributors impact the Movie Box office 
    performance?"
    
- Does the movie genre offered by distributors impact the Movie Box office 
    performance?"
    
## What data are we using for this?

# Dataset

The dataset used for this project came from Kaggle:

https://www.kaggle.com/datasets/sanjeetsinghnaik/top-1000-highest-grossing-movies

The data was updated 2 years ago by Sanjeet Singh Naik. This data was collected from 
various websites and combined for use in a variety of data operations. The information came from numerous websites, including IMDB, Rotten Tomatoes, and others.

The data is gathered for the public's use, particularly for those who are interested in 
movies because it contains a lot of observations (movies) and information about genre, duration, and sales value.

Producers and directors can use it to research audience preferences and current movie 
trends. Additionally, by collecting information on domestic and foreign sales, this data 
is also made for business purposes by giving investors a more thorough picture of the 
movie's revenue.

## Ethical Questions (Questions of Power)

- Is the dataset biased in any way that might favor a particular genre?

- Does the content of the movies reflect racism or sensitivity to a particular 
  culture or minority group?

## Limitations

Possible limitations and problems with this data is that there are some missing values 
in this data, which could potentially lead to biased results because the data does 
not fully represent the whole.  Furthermore, the dataset only includes movies from 
the United States, so it cannot be used to analyze other countries or to examine 
diversity.

# Conclusion / Summary Takeaways

## Big Picture Goal
    
The overall goal of our project is to guide film producers to creating the optimal 
film using distributor, runtime, rating, and genre as separate criteria for overall 
movie success, evaluated in world sales. 

### Takeaways

The main takeaway from this bar graph is that according to this dataset, a film 
director designing a movie script for any of these 4 ratings will make money on the 
same scale, but to optimize worldwide sales, they should significantly prioritize G, PG, 
& PG-13-rated films over R-rated films.

Out of all the individual genres in each dataset (and there were movies tagged with 
multiple), 'adventure' and 'action' were correlated with by far the two highest world 
sale incomes. Thus, a film director planning a movie should do so with the intention of an adventure or action theme, because the two genres have significant overlap and 
yield a lot of money in overall world sales.
        
For all the films in this top 1000 dataset, 'Warner Bros' and 'Walt Disney Studios 
Motion Pictures' were the two distributors who performed the best in not only overall 
world sales, but also world sales for PG-13 and G-rated movies, two of the higher-producing 
movie licenses. 

## Most Important Insight

The most significant pattern revealed from our analysis is that popular variables, 
such as genre, distributor, and rating have a much better relationship with movie 
success (once again, defined by world sales) in comparison to hidden variables, such as runtime and month of release.

This makes complete sense, because popular discourse around movies is split into a few common aspects: director (unavailable in this dataset), movie plot (available but not
really usable), distributor (when people usually say 'Disney movie'), rating (usually 
for families with kids), and genre, which semi-ties into the family element."),

Thus, the implication is that the way movies are contextualized in popular discourse 
could play a more significant role in their financial output than we think. Thus, 
prospective film directors could really take advantage of this facet when dictating the 
foundations of their films.
