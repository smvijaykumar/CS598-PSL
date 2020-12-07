#server.R

library(shiny)
library(proxy)
library(recommenderlab)
library(reshape2)
source("dataSorting.R")

myurl = "https://liangfgithub.github.io/MovieData/"



loadRatingData = function() {
  # use colClasses = 'NULL' to skip columns
  ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                     sep = ':',
                     colClasses = c('integer', 'NULL'), 
                     header = FALSE)
  colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
  ratings
}

loadMovieData = function() {
  movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
  movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
  movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
  movies = data.frame(movies, stringsAsFactors = FALSE)
  colnames(movies) = c('MovieID', 'Title', 'Genres')
  movies$MovieID = as.integer(movies$MovieID)
  
  # convert accented characters
  movies$Title[73]
  movies$Title = iconv(movies$Title, "latin1", "UTF-8")
  movies$Title[73]
  
  # extract year
  movies$Year = as.numeric(unlist(
    lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))
  
  genres = as.data.frame(movies$Genres, stringsAsFactors=FALSE)
  tmp = as.data.frame(tstrsplit(genres[,1], '[|]',
                                type.convert=TRUE),
                      stringsAsFactors=FALSE)
  genre_list = c("Action", "Adventure", "Animation", 
                 "Children's", "Comedy", "Crime",
                 "Documentary", "Drama", "Fantasy",
                 "Film-Noir", "Horror", "Musical", 
                 "Mystery", "Romance", "Sci-Fi", 
                 "Thriller", "War", "Western")
  m = length(genre_list)
  genre_matrix = matrix(0, nrow(movies), length(genre_list))
  for(i in 1:nrow(tmp)){
    genre_matrix[i,genre_list %in% tmp[i,]]=1
  }
  colnames(genre_matrix) = genre_list
  movies = cbind(movies,genre_matrix)
  movies
}

movies = loadMovieData()
ratings <- read.csv("ratings.csv")
movies <- movies[-which((movies$MovieID %in% ratings$MovieID) == FALSE),]

formatInput <- function(v,a,d){
  ## This function formats the user's input of Valence-Arousal-Dominance
  ## and outputs them as a vector
  
  c(v,a,d)
}

shinyServer(function(input, output) {
  
  output$ui <- renderUI({
    if (is.null(input$input_genre))
      return()
    #switch-case to display movies
    switch(input$input_genre,
           "Select" = tags$p("Select Genre #1 first to display movie list."),
           "Action" = selectInput("select", "Movie of Genre #1",
                                  choices = sort(subset(movies, Action == 1)$Title),
                                  selected = sort(subset(movies, Action == 1)$Title)[1]),
           "Adventure" = selectInput("select", "Movie of Genre #1",
                                     choices = sort(subset(movies, Adventure == 1)$Title),
                                     selected = sort(subset(movies, Adventure == 1)$Title)[1]),
           "Animation" =  selectInput("select", "Movie of Genre #1",
                                      choices = sort(subset(movies, Animation == 1)$Title),
                                      selected = sort(subset(movies, Animation == 1)$Title)[1]),
           "Children" =  selectInput("select", "Movie of Genre #1",
                                     choices = sort(subset(movies, Children == 1)$Title),
                                     selected = sort(subset(movies, Children == 1)$Title)[1]),
           "Comedy" =  selectInput("select", "Movie of Genre #1",
                                   choices = sort(subset(movies, Comedy == 1)$Title),
                                   selected = sort(subset(movies, Comedy == 1)$Title)[1]),
           "Crime" =  selectInput("select", "Movie of Genre #1",
                                  choices = sort(subset(movies, Crime == 1)$Title),
                                  selected = sort(subset(movies, Crime == 1)$Title)[1]),
           "Documentary" =  selectInput("select", "Movie of Genre #1",
                                        choices = sort(subset(movies, Documentary == 1)$Title),
                                        selected = sort(subset(movies, Documentary == 1)$Title)[1]),
           "Drama" =  selectInput("select", "Movie of Genre #1",
                                  choices = sort(subset(movies, Drama == 1)$Title),
                                  selected = sort(subset(movies, Drama == 1)$Title)[1]),
           "Fantasy" =  selectInput("select", "Movie of Genre #1",
                                    choices = sort(subset(movies, Fantasy == 1)$Title),
                                    selected = sort(subset(movies, Fantasy == 1)$Title)[1]),
           "Film.Noir" =  selectInput("select", "Movie of Genre #1",
                                      choices = sort(subset(movies, Film.Noir == 1)$Title),
                                      selected = sort(subset(movies, Film.Noir == 1)$Title)[1]),
           "Horror" =  selectInput("select", "Movie of Genre #1",
                                   choices = sort(subset(movies, Horror == 1)$Title),
                                   selected = sort(subset(movies, Horror == 1)$Title)[1]),
           "Musical" =  selectInput("select", "Movie of Genre #1",
                                    choices = sort(subset(movies, Musical == 1)$Title),
                                    selected = sort(subset(movies, Musical == 1)$Title)[1]),
           "Mystery" =  selectInput("select", "Movie of Genre #1",
                                    choices = sort(subset(movies, Mystery == 1)$Title),
                                    selected = sort(subset(movies, Mystery == 1)$Title)[1]),
           "Romance" =  selectInput("select", "Movie of Genre #1",
                                    choices = sort(subset(movies, Romance == 1)$Title),
                                    selected = sort(subset(movies, Romance == 1)$Title)[1]),
           "Sci.Fi" =  selectInput("select", "Movie of Genre #1",
                                   choices = sort(subset(movies, Sci.Fi == 1)$Title),
                                   selected = sort(subset(movies, Sci.Fi == 1)$Title)[1]),
           "Thriller" =  selectInput("select", "Movie of Genre #1",
                                     choices = sort(subset(movies, Thriller == 1)$Title),
                                     selected = sort(subset(movies, Thriller == 1)$Title)[1]),
           "War" =  selectInput("select", "Movie of Genre #1",
                                choices = sort(subset(movies, War == 1)$Title),
                                selected = sort(subset(movies, War == 1)$Title)[1]),
           "Western" = selectInput("select", "Movie of Genre #1",
                                   choices = sort(subset(movies, Western == 1)$Title),
                                   selected = sort(subset(movies, Western == 1)$Title)[1])
    )
  })
  
  output$ui2 <- renderUI({
    if (is.null(input$input_genre2))
      return()
    
    switch(input$input_genre2,
           "Select" = tags$p("Select Genre #2 first to display movie list."),
           "Action" = selectInput("select2", "Movie of Genre #2",
                                  choices = sort(subset(movies, Action == 1)$Title),
                                  selected = sort(subset(movies, Action == 1)$Title)[1]),
           "Adventure" = selectInput("select2", "Movie of Genre #2",
                                     choices = sort(subset(movies, Adventure == 1)$Title),
                                     selected = sort(subset(movies, Adventure == 1)$Title)[1]),
           "Animation" =  selectInput("select2", "Movie of Genre #2",
                                      choices = sort(subset(movies, Animation == 1)$Title),
                                      selected = sort(subset(movies, Animation == 1)$Title)[1]),
           "Children" =  selectInput("select2", "Movie of Genre #2",
                                     choices = sort(subset(movies, Children == 1)$Title),
                                     selected = sort(subset(movies, Children == 1)$Title)[1]),
           "Comedy" =  selectInput("select2", "Movie of Genre #2",
                                   choices = sort(subset(movies, Comedy == 1)$Title),
                                   selected = sort(subset(movies, Comedy == 1)$Title)[1]),
           "Crime" =  selectInput("select2", "Movie of Genre #2",
                                  choices = sort(subset(movies, Crime == 1)$Title),
                                  selected = sort(subset(movies, Crime == 1)$Title)[1]),
           "Documentary" =  selectInput("select2", "Movie of Genre #2",
                                        choices = sort(subset(movies, Documentary == 1)$Title),
                                        selected = sort(subset(movies, Documentary == 1)$Title)[1]),
           "Drama" =  selectInput("select2", "Movie of Genre #2",
                                  choices = sort(subset(movies, Drama == 1)$Title),
                                  selected = sort(subset(movies, Drama == 1)$Title)[1]),
           "Fantasy" =  selectInput("select2", "Movie of Genre #2",
                                    choices = sort(subset(movies, Fantasy == 1)$Title),
                                    selected = sort(subset(movies, Fantasy == 1)$Title)[1]),
           "Film.Noir" =  selectInput("select2", "Movie of Genre #2",
                                      choices = sort(subset(movies, Film.Noir == 1)$Title),
                                      selected = sort(subset(movies, Film.Noir == 1)$Title)[1]),
           "Horror" =  selectInput("select2", "Movie of Genre #2",
                                   choices = sort(subset(movies, Horror == 1)$Title),
                                   selected = sort(subset(movies, Horror == 1)$Title)[1]),
           "Musical" =  selectInput("select2", "Movie of Genre #2",
                                    choices = sort(subset(movies, Musical == 1)$Title),
                                    selected = sort(subset(movies, Musical == 1)$Title)[1]),
           "Mystery" =  selectInput("select2", "Movie of Genre #2",
                                    choices = sort(subset(movies, Mystery == 1)$Title),
                                    selected = sort(subset(movies, Mystery == 1)$Title)[1]),
           "Romance" =  selectInput("select2", "Movie of Genre #2",
                                    choices = sort(subset(movies, Romance == 1)$Title),
                                    selected = sort(subset(movies, Romance == 1)$Title)[1]),
           "Sci.Fi" =  selectInput("select2", "Movie of Genre #2",
                                   choices = sort(subset(movies, Sci.Fi == 1)$Title),
                                   selected = sort(subset(movies, Sci.Fi == 1)$Title)[1]),
           "Thriller" =  selectInput("select2", "Movie of Genre #2",
                                     choices = sort(subset(movies, Thriller == 1)$Title),
                                     selected = sort(subset(movies, Thriller == 1)$Title)[1]),
           "War" =  selectInput("select2", "Movie of Genre #2",
                                choices = sort(subset(movies, War == 1)$Title),
                                selected = sort(subset(movies, War == 1)$Title)[1]),
           "Western" = selectInput("select2", "Movie of Genre #2",
                                   choices = sort(subset(movies, Western == 1)$Title),
                                   selected = sort(subset(movies, Western == 1)$Title)[1])
    )
  })
  
  output$ui3 <- renderUI({
    if (is.null(input$input_genre3))
      return()
    
    switch(input$input_genre3,
           
           "Select" = tags$p("Select Genre #3 first to display movie list."),
           "Action" = selectInput("select3", "Movie of Genre #3",
                                  choices = sort(subset(movies, Action == 1)$Title),
                                  selected = sort(subset(movies, Action == 1)$Title)[1]),
           "Adventure" = selectInput("select3", "Movie of Genre #3",
                                     choices = sort(subset(movies, Adventure == 1)$Title),
                                     selected = sort(subset(movies, Adventure == 1)$Title)[1]),
           "Animation" =  selectInput("select3", "Movie of Genre #3",
                                      choices = sort(subset(movies, Animation == 1)$Title),
                                      selected = sort(subset(movies, Animation == 1)$Title)[1]),
           "Children" =  selectInput("select3", "Movie of Genre #3",
                                     choices = sort(subset(movies, Children == 1)$Title),
                                     selected = sort(subset(movies, Children == 1)$Title)[1]),
           "Comedy" =  selectInput("select3", "Movie of Genre #3",
                                   choices = sort(subset(movies, Comedy == 1)$Title),
                                   selected = sort(subset(movies, Comedy == 1)$Title)[1]),
           "Crime" =  selectInput("select3", "Movie of Genre #3",
                                  choices = sort(subset(movies, Crime == 1)$Title),
                                  selected = sort(subset(movies, Crime == 1)$Title)[1]),
           "Documentary" =  selectInput("select3", "Movie of Genre #3",
                                        choices = sort(subset(movies, Documentary == 1)$Title),
                                        selected = sort(subset(movies, Documentary == 1)$Title)[1]),
           "Drama" =  selectInput("select3", "Movie of Genre #3",
                                  choices = sort(subset(movies, Drama == 1)$Title),
                                  selected = sort(subset(movies, Drama == 1)$Title)[1]),
           "Fantasy" =  selectInput("select3", "Movie of Genre #3",
                                    choices = sort(subset(movies, Fantasy == 1)$Title),
                                    selected = sort(subset(movies, Fantasy == 1)$Title)[1]),
           "Film.Noir" =  selectInput("select3", "Movie of Genre #3",
                                      choices = sort(subset(movies, Film.Noir == 1)$Title),
                                      selected = sort(subset(movies, Film.Noir == 1)$Title)[1]),
           "Horror" =  selectInput("select3", "Movie of Genre #3",
                                   choices = sort(subset(movies, Horror == 1)$Title),
                                   selected = sort(subset(movies, Horror == 1)$Title)[1]),
           "Musical" =  selectInput("select3", "Movie of Genre #3",
                                    choices = sort(subset(movies, Musical == 1)$Title),
                                    selected = sort(subset(movies, Musical == 1)$Title)[1]),
           "Mystery" =  selectInput("select3", "Movie of Genre #3",
                                    choices = sort(subset(movies, Mystery == 1)$Title),
                                    selected = sort(subset(movies, Mystery == 1)$Title)[1]),
           "Romance" =  selectInput("select3", "Movie of Genre #3",
                                    choices = sort(subset(movies, Romance == 1)$Title),
                                    selected = sort(subset(movies, Romance == 1)$Title)[1]),
           "Sci.Fi" =  selectInput("select3", "Movie of Genre #3",
                                   choices = sort(subset(movies, Sci.Fi == 1)$Title),
                                   selected = sort(subset(movies, Sci.Fi == 1)$Title)[1]),
           "Thriller" =  selectInput("select3", "Movie of Genre #3",
                                     choices = sort(subset(movies, Thriller == 1)$Title),
                                     selected = sort(subset(movies, Thriller == 1)$Title)[1]),
           "War" =  selectInput("select3", "Movie of Genre #3",
                                choices = sort(subset(movies, War == 1)$Title),
                                selected = sort(subset(movies, War == 1)$Title)[1]),
           "Western" = selectInput("select3", "Movie of Genre #3",
                                   choices = sort(subset(movies, Western == 1)$Title),
                                   selected = sort(subset(movies, Western == 1)$Title)[1])
    )
  })
  
  #to display output data
  output$table <- renderTable({
    movie_recommendation(input$select, input$select2, input$select3)
  })
  
  output$dynamic_value <- renderPrint({
    c(input$select,input$select2,input$select3)
  })
  
})