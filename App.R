library(shiny); library(shinydashboard)
library(purrr); library(dplyr)

source("helpers.R")

ui <- shinydashboard::dashboardPage(
  header = shinydashboard::dashboardHeader(
    title = "Clue Assist?!"
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
                                    choices = clue_master_list$who)
          ),
          column(width = 3,
                 checkboxGroupInput("set_game_what_pub" , label = "What?",
                                    choices = clue_master_list$what)
          ),
          column(width = 3,
                 checkboxGroupInput("set_game_where_pub" , label = "Where?",
                                    choices = clue_master_list$where)
          )
        )
      ),
      h3("What cards have you been dealt?"),
      fluidRow(
        column(width = 3,
               checkboxGroupInput("set_game_who" , label = "Who?",
                                  choices = clue_master_list$who)
               ),
        column(width = 3,
               checkboxGroupInput("set_game_what" , label = "What?",
                                  choices = clue_master_list$what)
        ),
        column(width = 3,
               checkboxGroupInput("set_game_where" , label = "Where?",
                                  choices = clue_master_list$where)
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
        tabPanel(title = "Rumors",
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
                 ),
                 fluidRow(
                   column(width = 12, tableOutput("clue_tracker_table"))
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
  
  #Which player are you?
  player_list_me <- reactive({
    req(player_list())
    p_list <- sapply(1:as.numeric(input$set_game_players), function(i){
      sel_player <- input[[paste0("set_game_players_", i, "_me")]]
      
      return(sel_player)
    })
    return(player_list()[p_list])
  })
  
  ####
  # TRACK TURNS ----
  ####
  observeEvent(input$turn_add, {

    insertUI(
      selector = "#turn_add",
      where = "afterEnd",
      ui = tagList(
        fluidRow(
          column(width = 1, h2(input$turn_add)),
          column(width = 3, selectInput(paste0("turn_", input$turn_add, "_guessedby"),
                                        label = "Who's turn?", choices = list("Guessed By" = "none", "Player" = player_list())))
        ),
        fluidRow(
          column(width = 1),
          column(width = 3, selectInput(paste0("turn_", input$turn_add, "_who"),
                                        label = "", choices = list("Who?" = "none", "Who?" = clue_master_list$who))),
          column(width = 3, selectInput(paste0("turn_", input$turn_add, "_what"),
                                        label = "", choices = list("What?" = "none", "What?" = clue_master_list$what))),
          column(width = 3, selectInput(paste0("turn_", input$turn_add, "_where"),
                                        label = "", choices = list("Where?" = "none", "Where?" = clue_master_list$where)))),
        fluidRow(
          column(width = 1),
          column(width = 3, selectInput(paste0("turn_", input$turn_add, "_disprovedby"),
                                        label = "", choices = list("Disproved By" = "none", "Player" = player_list()))),
          column(width = 3, selectInput(paste0("turn_", input$turn_add, "_disprovedclue"),
                                        label = "", choices = list("Disproved Clue" = "none", "Clue" = clue_master_list)))
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
  
  turn_tracker2 <- reactive({
    req(turn_tracker())
    
    player_list <- player_list()
    n_players <- length(player_list)
    
    dat <- turn_tracker() %>% 
      #Players who failed to disprove a rumor
      rowwise() %>% 
      mutate(
        guessedby2 = ifelse(guessedby == "none", 0, which(player_list == guessedby)),
        disprovedby2 = ifelse(disprovedby == "none", 0, which(player_list == disprovedby))) %>% 
      mutate(cannot_disprove = map2(guessedby2, disprovedby2, function(a, b){
        calc_player_dist(a, b, n_players, player_list)
      })) %>% 
      ungroup()
    
    return(dat)
  })
  
  output$turn_tracker_table <- renderTable({
    req(turn_tracker())
    dat <- turn_tracker() %>% 
      select(Turn = turn, `Who?` = who, `What?` = what, `Where?` = where, `Guessed by` = guessedby, `Disproved by` = disprovedby, `Diproved Clue` = disprovedclue) %>% 
      mutate_if(is.character, funs(ifelse(. == "none", "", .)))
    
    return(head(dat, 5))
  })
  
  ####
  # Clue Probabilities ----
  ####
  
  rumor_prior <- reactive({
    clue_list <- list()
    
    disproved_clues <- if(is.null(turn_tracker())){NULL}else{turn_tracker()$disprovedclue}
    
    #Who
    set_who <- c(input$set_game_who, input$set_game_who_pub, disproved_clues)
    clue_list[["who"]] <- !(clue_master_list$who %in% set_who)
    names(clue_list[["who"]]) <- clue_master_list$who
    
    #What
    set_what <- c(input$set_game_what, input$set_game_what_pub, disproved_clues)
    clue_list[["what"]] <- !(clue_master_list$what %in% set_what)
    names(clue_list[["what"]]) <- clue_master_list$what
    
    #Where
    set_where <- c(input$set_game_where, input$set_game_where_pub, disproved_clues)
    clue_list[["where"]] <- !(clue_master_list$where %in% set_where)
    names(clue_list[["where"]]) <- clue_master_list$where
    
    lapply(clue_list, function(x){x/sum(x)})
  })
  
  
  ###
  # Individual clue trackers
  ###
  clue_tracker_by_me <- reactive({
    game_who <- input$set_game_who
    if(length(game_who) == 0){game_who <- "none"}
    game_what <- input$set_game_what
    if(length(game_what) == 0){game_what <- "none"}
    game_where <- input$set_game_where
    if(length(game_where) == 0){game_where <- "none"}
    
    clues_possessed_by_me <- tibble::tibble(clue_type = "who", clue = game_who) %>% 
      bind_rows(tibble::tibble(clue_type = "what", clue = game_what)) %>% 
      bind_rows(tibble::tibble(clue_type = "where", clue = game_where)) %>% 
      filter(clue != "none") %>% 
      mutate(player = player_list_me())
    
    return(clues_possessed_by_me)
  })
  
  clue_tracker_by_public <- reactive({
    game_who <- input$set_game_who_pub
    if(length(game_who) == 0){game_who <- "none"}
    game_what <- input$set_game_what_pub
    if(length(game_what) == 0){game_what <- "none"}
    game_where <- input$set_game_where_pub
    if(length(game_where) == 0){game_where <- "none"}
    
    clues_possessed_by_public <- tibble::tibble(clue_type = "who", clue = game_who) %>% 
      bind_rows(tibble::tibble(clue_type = "what", clue = game_what)) %>% 
      bind_rows(tibble::tibble(clue_type = "where", clue = game_where)) %>% 
      filter(clue != "none") %>% 
      mutate(player = ".PublicClues")
    
    return(clues_possessed_by_public)
  })
  
  clue_tracker <- reactive({
    req(turn_tracker2())
    
    dat <- turn_tracker2()
    player_list <- player_list()
    
    #3-Series data frames... clue_type, clue, player
    clues_possessed_by_me <- clue_tracker_by_me()
    clues_possessed_by_public <- clue_tracker_by_public()
    
    #Clues Possessed, as learned from reveals to Me
    clues_possessed_by_others <- dat %>% 
      filter(disprovedclue != "none") %>% 
      select(player = disprovedby, clue = disprovedclue) %>% 
      distinct() %>% 
      mutate(clue_type = case_when(
        clue %in% clue_master_list$who ~ "who",
        clue %in% clue_master_list$what ~ "what", 
        clue %in% clue_master_list$where ~ "where",
        TRUE ~ "error"
      ))
    
    #Clues MAYBE possessed by others... 
    #... when someone privately disproves a rumor, theres a 33%, 50%, or 100% chance we know the clue
    clues_maybe_possessed <- dat %>% 
      filter(disprovedclue == "none") %>% 
      select(-guessedby, -cannot_disprove, -disprovedclue) %>% 
      rename(player = disprovedby) %>% 
      gather(clue_type, clue, who, what, where) %>% 
      arrange(turn) %>% 
      count(player, clue_type, clue)
    
    #Create a 3-series data frame of each card NOT possessed by players
    clues_not_possessed <- dat %>% 
      unnest(cannot_disprove) %>% 
      select(player = cannot_disprove, who, what, where) %>% 
      filter(player != "") %>% 
      gather(clue_type, clue, who, what, where) %>% 
      distinct() %>% 
      arrange(player, clue_type)
    
    #BRING TOGETHER INTO A SINGLE ARRAY
    clue_tracker <- tibble::tibble(clue_type = "who", clue = clue_master_list$who) %>% 
      bind_rows(tibble::tibble(clue_type = "what", clue = clue_master_list$what)) %>% 
      bind_rows(tibble::tibble(clue_type = "where", clue = clue_master_list$where)) %>% 
      mutate(player = ".Envelope") %>% 
      complete(nesting(clue_type, clue), player = c(player_list, ".PublicClues", ".Envelope")) %>% 
      #Bring in known clues
      left_join(bind_rows(list(clues_possessed_by_me, 
                               clues_possessed_by_public, 
                               clues_possessed_by_others)) %>% 
                  distinct() %>% 
                  mutate(.verified = 1),
                by = c("clue_type", "clue", "player")) %>% 
      #Bring in possible clues
      left_join(clues_maybe_possessed %>% rename(.possible = n),
                by = c("clue_type", "clue", "player")) %>% 
      #Bring in known NON possessions
      left_join(clues_not_possessed %>% mutate(.notpossessed = 0),
                by = c("clue_type", "clue", "player")) %>% 
      #Check for issues
      mutate(flag = .verified == 1 & .notpossessed == 0) %>% 
      #Go over what we know by clue
      group_by(clue) %>% 
      #Clean up NAs in Verified if possible
      mutate(.verified2 = ifelse(is.na(.verified) & any(.verified == 1), 0, .verified),
             #If no infomation, gets a unit probability. Each additional rumor disproven adds a unit
             .possible2 = ifelse(is.na(.possible), 1, 2^(.possible)),
             #If Public Clue or ME doesn't have a card, we know they don't possess it
             .notpossessed2 = ifelse(player == ".PublicClues"   & is.na(.verified), 0, .notpossessed),
             .notpossessed2 = ifelse(player == player_list_me() & is.na(.verified), 0, .notpossessed2),
             .notpossessed2 = ifelse(is.na(.notpossessed2), 1, 0),
             .prob_scaled = ifelse(is.na(.verified2), .possible2 * .notpossessed2, .verified2 * .possible2)) %>% 
      # THIS GIVES US INDEPENDENT PROBABILITIES FOR EACH CLUE
      ungroup()
    
    #Iterative scaling for probabilities... tying Clue distribution to 1 and PLayer distribution to N
    clue_tracker2 <- clue_tracker %>% 
      scale_to_clue() %>% 
      scale_to_player(player_list) %>% 
      scale_to_clue() %>% 
      scale_to_player(player_list) %>% 
      scale_to_clue() %>% 
      scale_to_player(player_list) %>% 
      scale_to_clue() %>% 
      scale_to_player(player_list) %>%
      mutate(.prob_scaled = round(.prob_scaled, 0))
    
    clue_tracker_spread <- clue_tracker2 %>% 
      select(clue_type, clue, player, .prob_scaled) %>% 
      spread(player, .prob_scaled)
    
    return(clue_tracker_spread)
  })
  
  output$clue_tracker_table <- renderTable(
    clue_tracker()
  )
  #Print Table - redo this later
  
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
