# Summer App

# load packages
library(shiny) #app
library(shinyMobile) #app for smartphone
library(DT) #tables
library(tidyverse) #data management

# color theme
col_load <- "purple"
col_start <- "deeporange"
#getF7Colors()

# app
shinyApp(
  ui = f7Page(
    f7SingleLayout(
      navbar = f7Navbar(
        title = "Summing scores for lazy players",
        hairline = FALSE,
        shadow = TRUE
      ),
      
      # main content
      # start card
      f7ExpandableCard(
        id = "startcard",
        title = "LET'S PLAY!!!",
        subtitle = "Who wants to play?",
        color = col_start,
        fullBackground = FALSE,
        lapply(1:8,
               function(x){
                 f7Text(inputId = paste0("player", x),
                        label = NULL,
                        placeholder = paste("Name Player", x))
               }), 
        f7Button(
            inputId = "startbutton",
            color = col_start, 
            label = "Start game")
      ), 
      # load card
      f7ExpandableCard(
        id = "loadcard",
        title = "Saved Games",
        subtitle = "Do you want to continue playing?",
        color = col_load,
        fullBackground = FALSE
      ),
      f7Popup(
        id = "popup1",
        title = "My first popup",
        f7Text("text", "Popup content", "This is my first popup ever, I swear!")
      )
    )
  ),
  
  server = function(input, output, session) {
    
    observeEvent(input$startbutton, {
      updateF7Popup(id = "popup1")
    })
    
  }
)


