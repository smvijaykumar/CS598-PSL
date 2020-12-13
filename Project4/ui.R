#ui.R
library(shiny)
library(dplyr)
library(shinycssloaders)
library(shinyjs)
library(ShinyRatingInput)

jsCode <-"shinyjs.reset_1 = function(params){$('.rating-symbol-foreground').css('width', params);}"

#static array for genre list
genre_list <- c("Select","Action", "Adventure", "Animation", "Childrens", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci.Fi", "Thriller", "War", "Western")



#shiny code to display web page
shinyUI(fluidPage(
  useShinyjs(),
  extendShinyjs(text = jsCode,functions = "reset_1"),
  tags$head(
    tags$style(
      HTML(".shiny-notification {
              height: 100px;
              width: 600px;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(50% - 200px);;
            }
           "
      )
    )
  ),
  tabsetPanel( id = "tabs",
     tabPanel("System-1",wellPanel("Movie Recommendation Engine - System 1"),
              tags$style("body {background: url(http://www.wallpaperup.com/wallpaper/download/858715) no-repeat center center fixed; 
                         background-size: cover;   filter:grayscale(100%);}"),
              fluidRow(
                
                column(3, wellPanel(h4("Select Movie Genres You Like")),
                       #wellPanel(tableOutput("ui4"))
                      uiOutput("renderGenres")
                ),
                
                column(9,
                       wellPanel(h4("Popular Movies with higher Avg Rating for selected Genres!")),
                       wellPanel(tableOutput("table2") %>% withSpinner(color="#0dc5c1")) 
                )
              )
     ),        
               
    tabPanel("System-2",
              wellPanel("Movie Recommendation Engine - System 2"),
              tags$style("body {background: url(http://www.wallpaperup.com/wallpaper/download/858715) no-repeat center center fixed; 
                         background-size: cover;   filter:grayscale(100%);}"),

             fluidRow(
                
                column(7, wellPanel(h4("Rate Movies You Like and then Click Button")),
                       uiOutput("recommenderButton"),
                       uiOutput("renderMoviesForRatings")
                       ),
                
                column(5,
                       wellPanel(h4("You Might Like The Following Movies!")),
                       wellPanel(tableOutput("table")  %>% withSpinner(color="#0dc5c1") ))
              )
      )
    
  )
))
