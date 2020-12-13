library(proxy)
library(recommenderlab)
library(reshape2)
#movies <- read.csv("movies.csv", header = TRUE, stringsAsFactors=FALSE)
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
  genre_list = c("Action", "Adventure", "Animation", 
                 "Childrens", "Comedy", "Crime",
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
  return(movies)
}
movies = loadMovieData()
ratings = read.csv("ratings.csv", header = TRUE)
movies_new = movies[-which((movies$MovieID %in% ratings$MovieID) == FALSE),]


movie_recommendation <- function(ratedMovieIds,ratedMovieIdsRatings) {

  selMovies = which(movies_new$MovieID == ratedMovieIds)
  
  
  ratingmatrix = dcast(ratings, UserID~MovieID, value.var = "Rating", na.rm=FALSE)
  ratingmatrix = ratingmatrix[,-1]
  
  userSelect = matrix(NA,ncol(ratingmatrix))
  userSelect[selMovies] = ratedMovieIdsRatings
  userSelect = t(userSelect)
  
  
  colnames(userSelect) = colnames(ratingmatrix)
  ratingmatrix_new = rbind(userSelect,ratingmatrix)
  ratingmatrix_new = as.matrix(ratingmatrix_new)
  
  #Convert rating matrix into a sparse matrix
  ratingmatrix_new <- as(ratingmatrix_new, "realRatingMatrix")
  
  userId = sample(1:6000,1)
  #Create Recommender Model. "UBCF" stands for user-based collaborative filtering
  recommender_model <- Recommender(ratingmatrix_new, method = "UBCF",param=list(method="Cosine",nn=5))
  recommender <- predict(recommender_model, ratingmatrix_new[userId], n=10)
  recom_list <- as(recommender, "list")
  no_result <- data.frame(matrix(NA,1))
  recom_result <- data.frame(matrix(NA,10))
  
  
  if (as.character(recom_list[1])=='character(0)'){
    no_result[1,1] <- "There are no similar movies in the dataset based on the movies you have selected. Please select different combinations.!!"
    colnames(no_result) <- "No results"
    return(no_result) 
  } else {
    for (i in c(1:10)){
      recom_result[i,1] <- as.character(subset(movies, 
                                               movies$MovieID == as.integer(recom_list[[1]][i]))$Title)
    }
    colnames(recom_result) <- "User-Based Collaborative Filtering Recommended Movies"
    return(recom_result)
  }
}
movie_recommendation_popular <- function(selectedGenres) {
  
  ratingmatrix <- dcast(ratings, UserID~MovieID, value.var = "Rating", na.rm=FALSE)
  ratingmatrix <- ratingmatrix[,-1]
  #Convert rating matrix into a sparse matrix
  ratingmatrix_new <- as(as.matrix(ratingmatrix), "realRatingMatrix")
  
  #Create Recommender Model using POPULAR algorithm
  recommender_model <- Recommender(ratingmatrix_new, method = "POPULAR",param = list(normalize = "Z-score"))
  recommender <- predict(recommender_model, ratingmatrix_new, type="ratings")
  recom_mat <- as(recommender, "matrix")
  avg_movie_ratings = cbind(colnames(recom_mat),colMeans(recom_mat,na.rm=TRUE))  # calculate movie avg ratings
  colnames(avg_movie_ratings) = c("MovieID","avg_ratings")
  avg_movie_ratings_df = as.data.frame(avg_movie_ratings)
  avg_movie_ratings_df$avg_ratings=as.numeric(avg_movie_ratings_df$avg_ratings)
  
  movies_new_with_rating = merge(movies,avg_movie_ratings_df, by = "MovieID")
  
  no_result <- data.frame(matrix(NA,1))
  
  
  #movies_new_with_rating = movies_new_with_rating[order(-movies_new_with_rating$avg_ratings,movies_new_with_rating$Genres),]
  returnList = list()
  columnNames = c()
  for(input in selectedGenres) {
    tmp = subset(movies_new_with_rating,eval(parse(text=paste(input,"== 1"))),select = c('MovieID','Title','avg_ratings'))
    returnList = cbind(returnList, tmp[order(-tmp$avg_ratings),][1:5,]$Title)
    columnNames = c(columnNames,input)
  }
  
  if (length(returnList) == 0){
    no_result[1,1] <- "There are no similar movies in the dataset based on the movies you have selected. Please select different combinations.!!"
    colnames(no_result) <- "No results"
    return(no_result) 
  } else {
    colnames(returnList) = columnNames
    return(returnList)
  }
}