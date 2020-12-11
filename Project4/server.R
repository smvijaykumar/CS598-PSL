#server.R

library(shiny)
library(proxy)
library(recommenderlab)
library(reshape2)
library(data.table)

myurl = "https://liangfgithub.github.io/MovieData/"

loadRatingData = function() {
  # use colClasses = 'NULL' to skip columns
  ratings = read.csv('ratings.dat', 
                     sep = ':',
                     colClasses = c('integer', 'NULL'), 
                     header = FALSE)
  colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
  ratings
}

#static array for genre list
genre_list <- c("Select","Action", "Adventure", "Animation", "Childrens", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci.Fi", "Thriller", "War", "Western")


loadMovieData = function() {
  movies = readLines('movies.dat')
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
  
  genres = as.data.frame(gsub("'","",movies$Genres), stringsAsFactors=FALSE)
  tmp = as.data.frame(tstrsplit(genres[,1], '[|]',
                                type.convert=TRUE),
                      stringsAsFactors=FALSE)

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

source("recommendation.R")

formatInput <- function(v,a,d){
  c(v,a,d)
}

shinyServer(function(input, output) {

  output$ui <- renderUI({
    if (input$input_genre == 'Select')
      return()

    #switch-case to display movies
    fluidRow(
      column(8,
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
           "Childrens" =  selectInput("select", "Movie of Genre #1",
                                     choices = sort(subset(movies, Childrens == 1)$Title),
                                     selected = sort(subset(movies, Childrens == 1)$Title)[1]),
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
           
        )),column(4,
              selectInput("select_rating","Provide Rating(1 to 5)",c(1,2,3,4,5))))
  })
  
  output$ui2 <- renderUI({
    if (input$input_genre2 == 'Select')
      return()

    fluidRow(
      column(8,
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
           "Childrens" =  selectInput("select2", "Movie of Genre #2",
                                     choices = sort(subset(movies, Childrens == 1)$Title),
                                     selected = sort(subset(movies, Childrens == 1)$Title)[1]),
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
        )), column(4,
                  selectInput("select2_rating","Provide Rating(1 to 5)",c(1,2,3,4,5))))
  })
  
  output$ui3 <- renderUI({
    if (input$input_genre3 == 'Select')
      return()
    fluidRow(
      column(8,
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
           "Childrens" =  selectInput("select3", "Movie of Genre #3",
                                     choices = sort(subset(movies, Childrens == 1)$Title),
                                     selected = sort(subset(movies, Childrens == 1)$Title)[1]),
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
           
    )), column(4,
    selectInput("select3_rating","Provide Rating(1 to 5)",c(1,2,3,4,5))))
  })
  output$ui31 = renderUI({
    if (is.null(input$select) && is.null(input$select2) && is.null(input$select3))
      return()
    fluidRow(column(3,actionButton("recBtn","Rate and Get Recommendation")))
  })
  
  output$ui4 <- renderUI({
    
    wellPanel(
      selectInput("select4", "Genre #1",
                  genre_list),
      selectInput("select5", "Genre #2",
                  genre_list),
      selectInput("select6", "Genre #3",
                  genre_list)
    )
     
  })
  final_output <- reactiveValues()
  
  observeEvent(input$recBtn, {

    withProgress(
      message = 'Getting Recommendation',
                 detail = 'This may take a while...', value = 0, {
                   final_output$rec_ucbf <- movie_recommendation(input$select, input$select2, 
                                                                 input$select3,
                                                                 input$select_rating,
                                                                 input$select2_rating,
                                                                 input$select3_rating)
                   for (i in 1:10) {
                     incProgress(1/10)
                     Sys.sleep(1)
                   }
                 })
    })
  
  #to display output data
  output$table <- renderTable({
    if (is.null(input$select) && is.null(input$select2) && is.null(input$select3))
      return()
    
    if(!is.null(final_output$rec_ucbf)) {
      return(final_output$rec_ucbf)
    } 
  })
  
  #to display output data
  output$table2 <- renderTable({
    if (is.null(input$select4) && is.null(input$select5) && is.null(input$select6))
      return()
    if(input$select4 != "Select" || input$select5 != "Select" || input$select6 != "Select")
      movie_recommendation_popular(input$select4, input$select5, input$select6)
  })
  
  output$dynamic_value <- renderPrint({
    c(input$select,input$select2,input$select3)
  })
  
})