library(shiny); library(shinydashboard)

clue <- list(
  who = c("Dorothy", "Blanche", "Rose", "Sophia", "Stan", "Miles"),
  what = c("Bathrobe", "Feathered Slipper", "Lipstick", "Ratan Chair", "Sophia's Purse", "Whipped Cream"),
  where = c("Dorothy's Bedroom", "Blanche's Bedroom", "Rose's Bedroom", "Sophia's Bedroom", 
            "The Kitchen", "The Bathroom",
            "The Lanai", "The Front Yard", "The Garage")
)

ui <- shinydashboard::dashboardPage(
  header = shinydashboard::dashboardHeader(
    title = "Clue Assist?"
  ),
  sidebar = shinydashboard::dashboardSidebar(disable = TRUE),
  body = shinydashboard::dashboardBody(
    ###
    # SET GAME CONDITIONS
    ###
    conditionalPanel(
      condition = "input.set_game == 0",
      h3("Who's playing?"),
      fluidRow(
        column( width = 12,
                radioButtons("set_game_players", label = "Number of players (including yourself!)",
                             choices = 3:6, selected = 3, inline = TRUE),
                uiOutput("set_game_players_names_ui")
        )
      ),
      conditionalPanel(
        condition = "input.set_game_players == 4 | input.set_game_players == 5",
        h3("What cards are public?"),
        fluidRow(
          column(width = 9,
                 helpText("In games with 4 or 5 players, after each player is dealt an equal sized hand, extra cards are placed on the table for all to see. 
                          Since all players know these cards, they have a different strategic value than the cards you are dealt.")
                 ),
          column(width = 3,
                 checkboxGroupInput("set_game_who_pub" , label = "Who?",
                                    choices = clue$who)
          ),
          column(width = 3,
                 checkboxGroupInput("set_game_what_pub" , label = "What?",
                                    choices = clue$what)
          ),
          column(width = 3,
                 checkboxGroupInput("set_game_where_pub" , label = "Where?",
                                    choices = clue$where)
          )
        )
      ),
      h3("What cards have you been dealt?"),
      fluidRow(
        column(width = 3,
               checkboxGroupInput("set_game_who" , label = "Who?",
                                  choices = clue$who)
               ),
        column(width = 3,
               checkboxGroupInput("set_game_what" , label = "What?",
                                  choices = clue$what)
        ),
        column(width = 3,
               checkboxGroupInput("set_game_where" , label = "Where?",
                                  choices = clue$where)
        )
      ), #End Row
      h3("What's your style?"),
      column(width = 12,
             radioButtons("set_game_style", label = "",
                          choices = c("Methodical" = "methodical", "Bold" = "bold")),
             conditionalPanel(condition = "input.set_game_style == 'methodical'",
                              HTML("You're a true Sherlock Holmes. Slowly chip away all the false rumors until you're left with the simple truth. <br/>
                                       <b>Pros:</b> You'll make calm, calculated guesses while keeping your cards and notes close to your chest. <br/>
                                       <b>Cons:</b> You're playing the long game. Someone might solve the crime before you!")
                              ),
             conditionalPanel(condition = "input.set_game_style == 'bold'",
                              HTML("Fish with dynamite. Keep your opponents guessing. <br/>
                                       <b>Pros:</b> Your game will be driven with bold, high probabilty guesses and the occassional self-incrimation to keep your opponents on their toes. <br/>
                                       <b>Cons:</b> This is a quick game. If you're too bold too often, your opponents will learn your hand.")
                              )
             ),
      actionButton("set_game", label = "Let's play!")
    ), #End game setup conditional panel
    
    ####
    # GAME UI
    ####
    conditionalPanel(condition = "input.set_game > 0",
      h2("Let's play!"),
      #Here - suggested guess for next turn
      #Here - table of  who, what, where with probabilities
      
      #Here - Rolling table of game turns
      h3("Game turns"),
      actionButton("turn_add", "Add a turn!")
    ) #End Game UI conditional panel
  )
)

server <- function(input, output, session) {
  
  ####
  #User Input set-up
  ###
  
  #PLayers 
  output$set_game_players_names_ui <- renderUI({
    lapply(1:as.numeric(input$set_game_players), function(i){
      list(
        fluidRow(
          column(width = 3,
                 textInput(inputId = paste0("sel_game_players_", i), 
                           label = paste("Player", i))
                 ),
          column(width = 9,
                 checkboxInput(inputId = paste0("sel_game_players_", i, "_me"), 
                               label = "This is me!")
                 )
        )
      )
    })
  }) #End play list
  
  #Who are these players?
  
  player_list <- reactive({
    p_list <- sapply(1:as.numeric(input$set_game_players), function(i){
      sel_player <- input[[paste0("sel_game_players_", i)]]
      
      if(sel_player == ""){sel_player <- paste("Player", i)}

      return(sel_player)
    })
    return(p_list)
  })
  
  ####
  # TRACK TURNS
  ####
  observeEvent(input$turn_add, {

    insertUI(
      selector = "#turn_add",
      where = "afterEnd",
      ui = tagList(
        fluidRow(
          column(width = 1, h2(input$turn_add)),
          column(width = 3, selectInput(paste0("turn_", input$turn_add, "_who"),
                                        label = "", choices = list("Who?" = "none", "Who?" = clue$who))),
          column(width = 3, selectInput(paste0("turn_", input$turn_add, "_what"),
                                        label = "", choices = list("What?" = "none", "What?" = clue$what))),
          column(width = 3, selectInput(paste0("turn_", input$turn_add, "_where"),
                                        label = "", choices = list("Where?" = "none", "Where?" = clue$where)))),
        fluidRow(
          column(width = 1),
          column(width = 3, selectInput(paste0("turn_", input$turn_add, "_guessedby"),
                                        label = "", choices = list("Guessed By" = "none", "Player" = player_list()))),
          column(width = 3, selectInput(paste0("turn_", input$turn_add, "_disprovedby"),
                                        label = "", choices = list("Disproved By" = "none", "Player" = player_list()))),
          column(width = 3, selectInput(paste0("turn_", input$turn_add, "_disprovedclue"),
                                        label = "", choices = list("Disproved Clue" = "none", "Clue" = clue)))
        ), #ENd Row
        hr()
      )
    )
  })
  
}

shinyApp(ui, server)
