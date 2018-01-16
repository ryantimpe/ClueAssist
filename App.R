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
  sidebar = shinydashboard::dashboardSidebar(),
  body = shinydashboard::dashboardBody(
    conditionalPanel(
      condition = "input.set_game == 0",
      h3("Who's playing?"),
      fluidRow(
        column( width = 12,
                radioButtons("set_game_players", label = "Number of players (including yourself!)",
                             choices = 2:6, selected = 4, inline = TRUE),
                uiOutput("set_game_players_names_ui")
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
    ) #End game setup conditional panel
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
  })
  
}

shinyApp(ui, server)
