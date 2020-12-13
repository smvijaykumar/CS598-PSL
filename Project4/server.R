#server.R

library(shiny)
library(proxy)
library(recommenderlab)
library(reshape2)
library(data.table)
library(shinyjs)
library(ShinyRatingInput)

myurl = "https://liangfgithub.github.io/MovieData/"
numGenres = 3

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

  final_output <- reactiveValues()
  
  observeEvent(
    input$recBtn, {
   
       ratedMovieIds = c()
       ratedMovieIdsRatings = c()
       for(i in randomMovieIds$ids) {
         if(input[[paste0("movieId",i)]] != "") {
           ratedMovieIds = c(ratedMovieIds,paste0("movieId",i))
           ratedMovieIdsRatings = c(ratedMovieIdsRatings,input[[paste0("movieId",i)]])
         }
         
       }
       final_output$rec_ucbf <- movie_recommendation(ratedMovieIds,ratedMovieIdsRatings)
    })
  
  observeEvent(input$addNew, { 
    numGenres = length(input_genres_r$InputGenres) + 1

    input_genres_r$InputGenres[[numGenres]] = 
      selectInput(paste0("input_genre",numGenres), 
                  paste0("Genre #",numGenres),
                  genre_list)

  })
  
  observeEvent(input$resetbtn, {
    #session$sendInputMessage("movieRating", list(value = NULL))
    js$reset_1(0)
    
  })
  
  #to display output data
  output$table <- renderTable({
    input$recBtn
    if(!is.null(final_output$rec_ucbf)) {
      return(final_output$rec_ucbf)
    } 
  })
  
  #to display output data
  output$table2 <- renderTable({
    numGenres = length(input_genres_r$InputGenres)
    if(numGenres == 0) return()
    getRec = FALSE
    selectedGenres = c()
    for(i in 1:numGenres) {
      if( !is.null(input[[paste0("input_genre",i)]]) && 
        input[[paste0("input_genre",i)]] != "Select" ) {
          getRec = TRUE
          selectedGenres = c(selectedGenres,input[[paste0("input_genre",i)]])
      }
    }
    if(getRec)
      return (movie_recommendation_popular(selectedGenres))
    return()
  })
  

  inputGenres = list(3)
  for(i in 1:numGenres) {
    inputGenres[[i]] = selectInput(paste("input_genre",sep = "",i), paste("Genre #","",i),
                                                  genre_list)
  }
  
  input_genres_r  = reactiveValues( InputGenres = inputGenres)
  
  output$recommenderButton = renderUI({
    wellPanel(
      actionButton("recBtn","Rate and Get Recommendation"),
      actionButton("resetbtn", "reset"))
  })
  
  output$renderGenres = renderUI( {
    wellPanel(
      do.call(fluidRow,input_genres_r$InputGenres),
      actionLink('addNew','Add More Genres')
    )})

    randomMovieIds = reactiveValues(ids=list())

  output$renderMoviesForRatings = renderUI( {
    
    if(length(randomMovieIds$ids) == 0) {
      randomMovieIds$ratingInputList = list()
      ids = list()
      for(i in 1:100) {
        id = sample(1:3600,1)
        ids[[i]] = movies_new[id,]$MovieID
        randomMovieIds$ratingInputList[[i]] = column(12,ratingInput(paste("movieId",sep="",movies_new[id,]$MovieID), label=movies_new[id,]$Title, dataStop=5))
      }
      randomMovieIds$ids = ids
    }

    wellPanel(
           do.call(fluidRow,randomMovieIds$ratingInputList)
    )})
  
 

})