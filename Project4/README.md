
<p align="center">
  <h3 align="center">Movie Recommendation Engine</h3>

  <p align="center">
   We are provided with a dataset contains about 1 million anonymous ratings of approximately 3,900 movies provided by 6,040 MovieLens users who joined MovieLens in 2000. The goal is to use the rating data to build a movie recommendation system based on a few different recommendation schemes, namely System I has two schemes based on movie genres, and System II has two schemes based on collaborative recommendation schemes.
    <br />
    <br />
    <a href="https://github.com/smvijaykumar/CS598-PSL/tree/master/Project4"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="http://40.85.185.52:3838/cs598-psl-project4/">View Demo</a>
    </p>
</p>

<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary><h2 style="display: inline-block">Table of Contents</h2></summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#acknowledgements">Acknowledgements</a></li>
  </ol>
</details>

<!-- ABOUT THE PROJECT -->
## About The Project

System I is based on movie genres

- One scheme recommend top 5 popular movies in each user selected genres. Popularity is measured by the number of ratings a movie receives. A popular movie may not be a highly rated movie. 

- The other scheme recommend 5 random movies in each user selected genres.

System II is based on collaborative filtering recommendation. 

- One scheme uses user-based collaborative filtering technique to estimate missing movie ratings based on other similar users ratings. Once missing movie ratings are estimated, movies with highest ratings is recommended at the top. 

- The other scheme uses item-based collaborative filtering technique to estimate missing movie ratings based on other movies similar to the onces that rated highly by the user. The most similar one is recommended to the top.

System I
<img src="https://github.com/smvijaykumar/CS598-PSL/blob/master/Project4/system1.PNG" alt="System I"/>
System II
<img src="https://github.com/smvijaykumar/CS598-PSL/blob/master/Project4/system1.PNG" alt="System II"/>


### Built With

* [R](R)
* [Recommenderlab](Recommenderlab)
* [Shiny](Shiny)
* [ShinyJS](ShinyJS)
* [DataTable](DataTable)
* [Reshape2](Reshape2)

<!-- GETTING STARTED -->
## Getting Started

To get a local copy up and running follow these simple steps.

### Prerequisites

This is an example of how to list things you need to use the software and how to install them.
* R (> 3.6.1)
* library(dplyr)
* library(ggplot2)
* library(recommenderlab)
* library(DT)
* library(data.table)
* library(reshape2)
* library(recommenderlab)
* library(Matrix)
* library(tidytable)
* library(knitr)
* library(shiny)
* library(shinyjs)
* library(shinyratinginput)


### Installation

1. Clone the repo
   ```sh
   git clone https://github.com/smvijaykumar/CS598-PSL/tree/master/Project4.git
   ```
2. Install R packages
  ```sh
install.packages(c("dplyr","ggplot2","DT","data.table","reshape2","recommenderlab","Matrix","tidytable","knitr","data.table","tidytable"))

install.packages(c("dplyr","rmarkdown","httpuv","shiny","shinythemes","shinycssloaders","shinyjs","shinyratinginput"))
   ```

<!-- USAGE EXAMPLES -->
## Usage

This is simple shinyapp for movie recommendation developed as part of Project 4 for course CS598-PSL. 

[ui.R](ui.R)

[server.R](server.R)

[recommendation.R](recommendation.R)

From RStudio, Click 'Run App' button to start Shiny App 
or
```sh
R -e "shiny::runApp('path to shinyapp')"
```
Run R Markdown:

Markdown File:  **Project_4_8742_vs24_VijayMatthew.Rmd**

This file contains Movies, Users and Ratings Data Exploration Analysis and the process of model building.

For the purpose of running, we evaluated the model only once in this markdown file.
We added another markdown file: **Parallel.RMD** which evaluates each model 10 times using parallel and foreach package to speed up the process.
<!-- Data-->
## Data
[Movie Rating Dataset](ratings.dat)

[Movie Movies Dataset](movies.dat)

[Movie Users Dataset](users.dat)

## Deployed App in Azure

We deployed our app in Azure cloud platform which runs 2 CPU , 8Cores virtual machine which runs on CENTOS.

 <a href="http://40.85.185.52:3838/cs598-psl-project4/">View Demo</a>
 
<!-- CONTACT -->
## Contact

Vijayakumar Sitha Mohan - vs24@illinois.edu
Waitong Matthew Leung   - wmleung2@illinois.edu

Project Link: [https://github.com/smvijaykumar/CS598-PSL/tree/master/Project4](https://github.com/smvijaykumar/CS598-PSL/tree/master/Project4)

<!-- ACKNOWLEDGEMENTS -->
## Acknowledgements

* Professor - Feng Liang
* TAs
