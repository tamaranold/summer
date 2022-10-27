# Summer App

# Sum score for turn base games

# install shinyMobile from github - hidden tab not working
#devtools::install_github("RinteRface/shinyMobile@rc-1.0.0")

# load packages
library(shiny) #app
library(shinyMobile) #app for smartphone
library(tidyverse) #data management

# color theme
col_load <- "purple"
col_start <- "deeporange"
#getF7Colors()
#f7Gallery()

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
        
        #score tab
        f7Tab(
          tabName = "hiddentab",
          hidden = TRUE,
          f7Card(
            #create scoreingboard including each named player
            f7List(
              uiOutput("scores")
            ),
            br(),
            #add round score to sum score
            f7Button(
              inputId = "addbutton",
              label = "Add scores"
            )
          )
        )))),
  
  server = function(input, output, session) {
    
    # list of all players
    players <- eventReactive(input$startbutton, {
       c(input$player1, input$player2, input$player3, input$player4, 
       input$player5, input$player6, input$player7, input$player8)[nchar(c(input$player1, input$player2, input$player3, input$player4, 
                                                                           input$player5, input$player6, input$player7, input$player8)) > 0]
    })
    
    #initiate scoreingboard
    observeEvent(input$startbutton,{
      updateF7Tabs(session = session, 
                   id = 'tabs', 
                   selected = 'hiddentab')
    })
    
    output$scores <- renderUI({
      req(length(players()) > 0)
      lapply(1:length(players()), function(j) {
        f7ListItem(
          #sum score
          f7Row(
            f7Col(
              f7Button(
                inputId = paste0("scorebutton", j),
                label = 0,
                size = "large"
              )),
            #score for each round
            f7Col(
              f7Text(
                inputId = paste0("roundscore", j),
                label = "",
                value = 0))),
          media = f7Icon("alarm_fill"),
          header = players()[j]
        )
      })
    })
    
   
   scores <- reactiveValues(player1 = 0,
                            player2 = 0,
                            player3 = 0,
                            player4 = 0)
   
   observeEvent(input$addbutton, {
     for(i in 1:length(players())){
       scores[[paste0("player", i)]]<- as.numeric(input[[paste0("roundscore", i)]]) +
         as.numeric(scores[[paste0("player", i)]])}
     })
    
   observeEvent(input$addbutton, {
    lapply(1:length(players()), function(j){ 
     updateF7Button(
      inputId = paste0("scorebutton", j),
      label = scores[[paste0("player", j)]]
    )
    
    updateF7Text(
      inputId = paste0("roundscore", j),
      value = 0
      ) 
    })
   })
   
   
   }
)
