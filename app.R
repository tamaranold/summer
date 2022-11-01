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
        
        #score tab
        f7Tab(
          tabName = "hiddentab",
          hidden = TRUE,
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
                f7Button(
                  inputId = "resetbutton",
                  label = "Reset game",
                  outline = TRUE,
                  fill = FALSE
                )
              ),
              #options
              f7Col(
                f7Button(
                  inputId = "optionsbutton",
                  label = "Options",
                  outline = TRUE,
                  fill = FALSE
                )
              ))
          ),
          f7Sheet(
            id = "optionssheet",
            label = "Highlights",
            orientation = "bottom",
            uiOutput("options")
            
            
          )
        )
      )
    )
  ),
  
  server = function(input, output, session) {
    ##start game (tabName = "Start")
    #get numbers of players from stepper
    number <- reactive({
      input$numberplayer
    })
    
    #create name slot for each player
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
    
    #list all players by name
    players <- eventReactive(input$startbutton, {
      p <- map_chr(1:number(), ~ input[[paste0("player", .)]])
      p[p != ""]
    })
    
    
    #initiate scoreingboard
    observeEvent(input$startbutton, {
      updateF7Tabs(session = session,
                   id = 'tabs',
                   selected = 'hiddentab')
    })
    
    ##sum scores (tabName = "hidden")
    #initiate scores for each player
    output$list <- renderUI({
      req(length(players()) > 0)
      
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
    
    #set start score for each player to 0
    scores <- reactiveValues()
    observeEvent(input$startbutton, {
      for (i in 1:length(players())) {
        scores[[paste0("player", i)]] <- 0
      }
    })
    
    #initiate summing of scores
    observeEvent(input$addbutton, {
      for (i in 1:length(players())) {
        scores[[paste0("player", i)]] <-
          as.numeric(input[[paste0("roundscore", i)]]) +
          as.numeric(scores[[paste0("player", i)]])
      }
    })
    
    #show new sumscore and reset roundscore to 0
    observeEvent(input$addbutton, {
      lapply(1:length(players()), function(j) {
        updateF7Button(inputId = paste0("scorebutton", j),
                       label = scores[[paste0("player", j)]])
        
        updateF7Text(inputId = paste0("roundscore", j),
                     value = 0)
      })
    })
    
    #button for resetting the game
    observeEvent(input$resetbutton, {
      lapply(1:length(players()), function(j) {
        updateF7Button(inputId = paste0("scorebutton", j),
                       label = 0)
        
        updateF7Text(inputId = paste0("roundscore", j),
                     value = 0)
      })
    })
    
    #button and inputs for highlighting
    observeEvent(input$optionsbutton, {
      updateF7Sheet(id = "optionssheet")
    })
    
    output$options <- renderUI({
      tagList(f7Row(f7Col(
        #highlight decreasing or increasing numbers
        f7Radio(
          inputId = "arrange",
          choices = c("The highest",
                      "The lowest"),
          selected = "The highest",
          label = "Which scores do you want to highlight?"
        )
      ),
      f7Col(
        #number of scores for highlighting
        f7Stepper(
          inputId = "numhigh",
          min = 0,
          max = length(players()),
          value = 0,
          label = ""
        )
      )))
    })
    
    #get position for highlighted scores
    poshigh <-
      eventReactive(input$numhigh |
                      input$addbutton | nchar(input$arrange), {
                        req(input$arrange)
                        req(input$numhigh)
                        
                        if (input$numhigh == 0) {
                          0
                        } else if (input$arrange == "The highest") {
                          s <- map_dbl(1:length(players()), ~ scores[[paste0("player", .)]])
                          which(s >= sort(s, decreasing = TRUE)[input$numhigh])
                        } else if (input$arrange == "The lowest") {
                          s <- map_dbl(1:length(players()), ~ scores[[paste0("player", .)]])
                          which(s <= sort(s)[input$numhigh])
                        }
                        
                      })
    
    #highlight highest/lowest scores
    observeEvent(poshigh(), {
      lapply(1:length(players()), function(x) {
        updateF7Button(inputId = paste0("scorebutton", x),
                       color = "deeppurple")
      })
      
      if (input$numhigh > 0) {
        lapply(poshigh(), function(x) {
          updateF7Button(inputId = paste0("scorebutton", x),
                         color = "pink")
        })
      }
    })
    
    
  }
)
