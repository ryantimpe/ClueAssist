library(shiny); library(shinydashboard)
library(purrr); library(dplyr)

#HELPERS TO BE MOVED LATER
clue <- list(
  who = c("Dorothy", "Blanche", "Rose", "Sophia", "Stan", "Miles"),
  what = c("Bathrobe", "Feathered Slipper", "Lipstick", "Ratan Chair", "Sophia's Purse", "Whipped Cream"),
  where = c("Dorothy's Bedroom", "Blanche's Bedroom", "Rose's Bedroom", "Sophia's Bedroom", 
            "The Kitchen", "The Bathroom",
            "The Lanai", "The Front Yard", "The Garage")
)

clue_tracker_table <- function(i, clue_list){
  if(clue_list[i] == 0){
    rw <- paste0("<tr>", "<td style='color:#999999'> ",
                 names(clue_list)[i], "</td> <td style='color:#999999'>", 
                 round(clue_list[i]*100), "%", " </td></tr>")
  } else if(clue_list[i] == 1){
    rw <- paste0("<tr>", "<td style='color:#4040FF; background-color:#FFFF40; font-weight:bold'> ",
                 names(clue_list)[i], "</td> <td style='color:#FF4040'>", 
                 round(clue_list[i]*100), "%", " </td></tr>")
  } else if(clue_list[i] == max(clue_list, na.rm=TRUE)){
    rw <- paste0("<tr>", "<td style='color:#FF4040; font-weight:bold'> ",
                 names(clue_list)[i], "</td> <td style='color:#FF4040'>", 
                 round(clue_list[i]*100), "%", " </td></tr>")
  } else {
    rw <- paste0("<tr><td>", names(clue_list)[i], " <td>", round(clue_list[i]*100), "%", " </tr>")
  }
  return(rw)
}


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
                 )
          ),
        fluidRow(
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
      tabsetPanel(
        tabPanel(title = "Spread a rumor",
                 icon = icon("user-circle"),
                 h3("Suspect Probabilities"),
                 fluidRow(
                   column(width = 12,
                          helpText("These probabilities are based off of all known information. This includes cards you have seen, rumors that have been disproven, and players who have declined to disprove a rumor.")
                   )
                 ),
                 fluidRow(
                   column(width = 3,
                          htmlOutput("probs_who")
                   ),
                   column(width = 3,
                          htmlOutput("probs_what")
                   ),
                   column(width = 3,
                          htmlOutput("probs_where")
                   )
                 )
                 ),
        tabPanel(title = "Game Turns",
                 icon = icon("question"),
                 h3("Last 5 Turns"),
                 tableOutput("turn_tracker_table"),
                 #actionButton("turn_log", "Update!"),
                 actionButton("turn_add", "Add a turn!")),
        tabPanel(title = "Player Hands",
                 icon = icon("users"))
      )
      #Here - suggested guess for next turn
      
      #Here - Rolling table of game turns

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
                 textInput(inputId = paste0("set_game_players_", i), 
                           label = paste("Player", i))
                 ),
          column(width = 9,
                 checkboxInput(inputId = paste0("set_game_players_", i, "_me"), 
                               label = "This is me!")
                 )
        )
      )
    })
  }) #End play list
  
  #Who are these players?
  
  player_list <- reactive({
    p_list <- sapply(1:as.numeric(input$set_game_players), function(i){
      sel_player <- input[[paste0("set_game_players_", i)]]
      
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
  }) #End turn tracker UI
  
  turn_tracker <- reactive({

    turn_add <- input$turn_add
    
    input_values <- reactiveValuesToList(input)
    
    if(input$turn_add == 0 | !any(grepl(paste0("turn_1_"), names(input_values)))){
      return(NULL)
    }
    
    turn_data <- tibble::tibble()
    for(i in 1:turn_add){
      dat <- dplyr::bind_cols(input_values[names(input_values)[grepl(paste0("turn_", i, "_"), names(input_values))]])
      names(dat) <- gsub(paste0("turn_", i, "_"), "", names(dat))
      
      turn_data <- turn_data %>% dplyr::bind_rows(dat %>% mutate(turn = i))
    }
    
    turn_data <- turn_data %>% dplyr::select(turn, who, what, where, 
                                             guessedby, disprovedby, disprovedclue, 
                                             everything()) %>% 
      dplyr::arrange(desc(turn))
    return(turn_data)
  })
  
  output$turn_tracker_table <- renderTable({
    req(turn_tracker())
    dat <- turn_tracker() %>% 
      rename(Turn = turn, `Who?` = who, `What?` = what, `Where?` = where, `Guessed by` = guessedby, `Disproved by` = disprovedby, `Diproved Clue` = disprovedclue) %>% 
      mutate_if(is.character, funs(ifelse(. == "none", "", .)))
    
    return(head(dat, 5))
  })
  
  ####
  # Clue Probabilities
  ####
  
  clue_tracker <- reactive({
    clue_list <- list()
    
    disproved_clues <- if(is.null(turn_tracker())){NULL}else{turn_tracker()$disprovedclue}
    
    #Who
    set_who <- c(input$set_game_who, input$set_game_who_pub, disproved_clues)
    clue_list[["who"]] <- !(clue$who %in% set_who)
    names(clue_list[["who"]]) <- clue$who
    
    #What
    set_what <- c(input$set_game_what, input$set_game_what_pub, disproved_clues)
    clue_list[["what"]] <- !(clue$what %in% set_what)
    names(clue_list[["what"]]) <- clue$what
    
    #Where
    set_where <- c(input$set_game_where, input$set_game_where_pub, disproved_clues)
    clue_list[["where"]] <- !(clue$where %in% set_where)
    names(clue_list[["where"]]) <- clue$where
    
    return(clue_list)
  })
  
  rumor_prior <- reactive({
    lapply(clue_tracker(), function(x){x/sum(x)})
  })
  
  output$probs_who <- renderText({
    set_wh <- sort(rumor_prior()$who, decreasing = TRUE)
    
    list_wh <- purrr::map(1:length(set_wh), function(x){clue_tracker_table(x, set_wh)})
    
    list_wh <- paste("<table><col width='130'><col width='80'>", 
                      paste(unlist(list_wh), collapse = " "), "</table>")
    
    return(list_wh)
  })
  output$probs_what <- renderText({
    set_wh <- sort(rumor_prior()$what, decreasing = TRUE)
    
    list_wh <- purrr::map(1:length(set_wh), function(x){clue_tracker_table(x, set_wh)})
    
    list_wh <- paste("<table><col width='130'><col width='80'>", 
                     paste(unlist(list_wh), collapse = " "), "</table>")
    
    return(list_wh)
  })
  output$probs_where <- renderText({
    set_wh <- sort(rumor_prior()$where, decreasing = TRUE)
    
    list_wh <- purrr::map(1:length(set_wh), function(x){clue_tracker_table(x, set_wh)})
    
    list_wh <- paste("<table><col width='130'><col width='80'>", 
                     paste(unlist(list_wh), collapse = " "), "</table>")
    
    return(list_wh)
  })
  
}

shinyApp(ui, server)
