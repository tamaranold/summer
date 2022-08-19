# Summer App

# load packages
library(shiny) #app
library(shinyMobile) #app for smartphone
library(DT) #tables
library(tidyverse) #data management

# color theme
col_back <- "#2A303A"
col_load <- "#F1CE32"
col_start <- "#892678"

# app
shinyApp(
  ui = f7Page(
    title = "Summing scores for lazy players",
    f7SingleLayout(
      navbar = f7Navbar(
        title = "Single Layout",
        hairline = TRUE,
        shadow = TRUE
      ),
      
      # main content
      # start card
      f7ExpandableCard(
        id = "startcard",
        title = "LET'S PLAY!!!",
        subtitle = "Who wants to play?",
        color = col_start,
        fullBackground = TRUE
      ), 
      # load card
      f7ExpandableCard(
        id = "loadcard",
        title = "Saved Games",
        subtitle = "Do you want to continue playing?",
        color = col_load,
        fullBackground = TRUE
      )
    )
  ),
  server = function(input, output) {
   
  }
)

