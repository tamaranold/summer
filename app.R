# Summer App

# install shinymobile from github - hidden tab not working
#devtools::install_github("RinteRface/shinyMobile@rc-1.0.0")

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
    f7TabLayout(
      navbar = f7Navbar(
        title = "Summing scores for lazy players",
        hairline = FALSE,
        shadow = TRUE
      ),
      f7Tabs(
        id = "tabs",
        swipeable = TRUE,
        animated = FALSE,
        
        #start tab
        f7Tab(
          tabName = "Start",
          icon = f7Icon("gamecontroller"),
          active = TRUE,
          
          #list of players
          f7Card(
            title = "Who wants to play?",
            lapply(1:8,
                   function(x){
                     f7Text(inputId = paste0("player", x),
                            label = NULL,
                            placeholder = paste("Name Player", x))
                   }),
            br(),
            
            #start button
            f7Button(
              inputId = "startbutton",
              color = col_start, 
              label = "Start game"))),
        
        #saved games tab
        f7Tab(
          tabName = "Saved",
          icon = f7Icon("floppy_disk"),
          f7Card(
            )),
        
        #table
        f7Tab(
          tabName = "hiddentab",
          hidden = TRUE,
          f7Card(
            DTOutput('tbl'),
                 br(),
                 #save button
                 f7Button(
                   inputId = "savebutton",
                   color = col_load, 
                   label = "Start game"))
        )))),
  
  server = function(input, output, session) {
    
    # list of all players
    players <- eventReactive(input$startbutton, {
       c(input$player1, input$player2, input$player3, input$player4, 
       input$player5, input$player6, input$player7, input$player8)[nchar(c(input$player1, input$player2, input$player3, input$player4, 
                                                                           input$player5, input$player6, input$player7, input$player8)) > 0]
    })
    
    # initiate dataframe for results
    data <- eventReactive(input$startbutton, {
      data.frame(matrix(ncol = length(players()),
                        dimnames = list(c(), players())))
    })
  
    # render result table
    output$tbl <- DT::renderDataTable(
      datatable(data(),
                style = 'bootstrap',
                class = "table-dark table-striped")
    )
   
    observeEvent(input$startbutton,{
      updateF7Tabs(session = session, 
                   id = 'tabs', 
                   selected = 'hiddentab')
    })
    

  }
)