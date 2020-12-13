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

Movie Recommendation Engine

<img src="https://github.com/smvijaykumar/CS598-PSL/blob/master/Project4/system1.PNG">System I</img>

<img src="https://github.com/smvijaykumar/CS598-PSL/blob/master/Project4/system1.PNG">System II</img>


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

<!-- Data-->
## Data
[Movie Rating Dataset](ratings.dat)
[Movie Movies Dataset](movies.dat)
[Movie Users Dataset](users.dat)

<!-- CONTACT -->
## Contact

Vijayakumar Sitha Mohan - vs24@illinois.edu
Waitong Matthew Leung   - wmleung2@illinois.edu

Project Link: [https://github.com/smvijaykumar/CS598-PSL/tree/master/Project4](https://github.com/smvijaykumar/CS598-PSL/tree/master/Project4)

<!-- ACKNOWLEDGEMENTS -->
## Acknowledgements

* Professor - Feng Liang
* TAs
