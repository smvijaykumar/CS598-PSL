#ui.R
library(shiny)
library(shinythemes)
library(dplyr)
library(shinycssloaders)


#static array for genre list
genre_list <- c("Select","Action", "Adventure", "Animation", "Childrens", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci.Fi", "Thriller", "War", "Western")



#shiny code to display web page
shinyUI(fluidPage(
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
                       wellPanel(tableOutput("ui4"))
                ),
                
                column(9,
                       wellPanel(h4("You Might Like The Following Highly Rated Movies!")),
                       wellPanel(tableOutput("table2") %>% withSpinner(color="#0dc5c1")) 
                )
              )
     ),        
               
    tabPanel("System-2",
              wellPanel("Movie Recommendation Engine - System 2"),
              tags$style("body {background: url(http://www.wallpaperup.com/wallpaper/download/858715) no-repeat center center fixed; 
                         background-size: cover;   filter:grayscale(100%);}"),
              fluidRow(
                
                column(3, wellPanel(h4("Select Movie Genres You Like :")),
                       wellPanel(
                         selectInput("input_genre", "Genre #1",
                                     genre_list),
                         selectInput("input_genre2", "Genre #2",
                                     genre_list),
                         selectInput("input_genre3", "Genre #3",
                                     genre_list)
                         
                       )),
                
                column(5,  wellPanel(h4("Select Movies You Like and Rate:")),
                       wellPanel(
                         # This outputs the dynamic UI component
                         uiOutput("ui"),
                         uiOutput("ui2"),
                         uiOutput("ui3"),
                         uiOutput("ui31") 
                        
                       )),
                column(4,
                       wellPanel(h4("You Might Like The Following Movies!")),
                       wellPanel(
                         tableOutput("table") 
                        
                       ))
              )
      )
    
  )
))
