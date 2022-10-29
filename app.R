# Summer App

# Sum score for turn base games

# install shinyMobile from github - hidden tab not working
#devtools::install_github("RinteRface/shinyMobile@rc-1.0.0")

# load packages
library(shiny) #app
library(shinyMobile) #app for smartphone
library(tidyverse) #data management

#getF7Colors()
#f7Gallery()

# app
shinyApp(
  ui = f7Page(
    options = list(
      theme = "md",
      dark = TRUE,
      color = "deeppurple"
    ),
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
            f7Stepper(
              inputId = "numberplayer",
              label = NULL,
              min = 1,
              max = 50,
              value = 4
            ),
            uiOutput("namesplayer"),
            br(),
            
            #start button
            f7Button(inputId = "startbutton",
                     label = "Start game")
          )
        ),
        
        #saved games tab
        f7Tab(tabName = "Saved",
              icon = f7Icon("floppy_disk"),
              f7Card()),
        
        #score tab
        f7Tab(
          tabName = "hiddentab",
          hidden = TRUE,
          f7Card(
            f7Row(
              f7Col(
                f7Stepper(
                  inputId = "numhigh",
                  min = 0,
                  max = 10,
                  value = 0,
                  label = "Color highest values"
                )    
              ),
              f7Col(
              f7Stepper(
                inputId = "numlow",
                min = 0,
                max = 10,
                value = 0,
                label = "Color lowest values"
              ))
            )
            
            ),
          f7Card(
            
            #create scoreingboard including each named player
            f7List(uiOutput("list"),
                   mode = "media"),
            br(),
            br(),
            #add round score to sum score
            f7Button(inputId = "addbutton",
                     label = "Add scores"),
            br(),
            f7Row(#reset game
              f7Col(
                f7Button(inputId = "resetbutton",
                         label = "Reset game",
                         outline = TRUE,
                         fill = FALSE)
              ),
              #save results
              f7Col(
                f7Button(inputId = "savebutton",
                         label = "Save results",
                         outline = TRUE,
                         fill = FALSE)
              ))
            
          )
        )
      )
    )
  ),
  
  server = function(input, output, session) {
    # list of all players
    players <- eventReactive(input$startbutton, {
      c(
        input$player1,
        input$player2,
        input$player3,
        input$player4,
        input$player5,
        input$player6,
        input$player7,
        input$player8
      )[nchar(
        c(
          input$player1,
          input$player2,
          input$player3,
          input$player4,
          input$player5,
          input$player6,
          input$player7,
          input$player8
        )
      ) > 0]
    })
    
    #name players
    number <- reactive({
      input$numberplayer
    })
    
    
    output$namesplayer <- renderUI({
      lapply(1:number(),
             function(x) {
               f7Text(
                 inputId = paste0("player", x),
                 label = NULL,
                 placeholder = paste("Name Player", x)
               )
             })
    })
    
    #initiate scoreingboard
    observeEvent(input$startbutton, {
      updateF7Tabs(session = session,
                   id = 'tabs',
                   selected = 'hiddentab')
    })
    
    output$list <- renderUI({
      req(length(players()) > 0)
      req(tbl)
      
      lapply(1:length(players()), function(j) {
        f7ListItem(#sum score
          f7Row(f7Col(
            f7Button(
              inputId = paste0("scorebutton", j),
              label = 0,
              size = "large"
            )
          ),
          #score for each round
          f7Col(
            f7Text(
              inputId = paste0("roundscore", j),
              label = "",
              value = 0
            )
          )),
          title = players()[j])
      })
    })
    
    
    #accept numbers only in input$roundscore
    #realtime update for input$roundscore
    #texts <- reactive({
    #  req(input$roundscore1)
    #  map(1:length(players()), ~ input[[paste0("roundscore", .x)]])
    #})
    
    #check input$roundscore for characters
    observeEvent(input$startbutton, {
      lapply(1:length(players()), function(j) {
        observe({
          validateF7Input(
            inputId = paste0("roundscore", j),
            pattern = "^-?[0-9]*",
            error = "Only numbers please!",
            session = session
          )
        })
      })
    })
    
    #start score
    scores <- reactiveValues(
      player1 = 0,
      player2 = 0,
      player3 = 0,
      player4 = 0,
      player5 = 0,
      player6 = 0,
      player7 = 0,
      player8 = 0
      
    )
    
    #sum score
    observeEvent(input$addbutton, {
      for (i in 1:length(players())) {
        scores[[paste0("player", i)]] <-
          as.numeric(input[[paste0("roundscore", i)]]) +
          as.numeric(scores[[paste0("player", i)]])
      }
    })
    
    observeEvent(input$addbutton, {
      lapply(1:length(players()), function(j) {
        updateF7Button(inputId = paste0("scorebutton", j),
                       label = scores[[paste0("player", j)]])
        
        updateF7Text(inputId = paste0("roundscore", j),
                     value = 0)
      })
    })
    
    #reset game
    observeEvent(input$resetbutton, {
      lapply(1:length(players()), function(j) {
        updateF7Button(inputId = paste0("scorebutton", j),
                       label = 0)
        
        updateF7Text(inputId = paste0("roundscore", j),
                     value = 0)
      })
    })
    
    #saving results
    observeEvent(input$savebutton, {
      
    })
    
  }
)
