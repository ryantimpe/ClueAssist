#HELPERS

#Series settings ----
clue_master_list <- list(
  who = c("Dorothy", "Blanche", "Rose", "Sophia", "Stan", "Miles"),
  what = c("Bathrobe", "Feathered Slipper", "Lipstick", "Rattan Chair", "Sophia's Purse", "Whipped Cream"),
  where = c("Dorothy's Bedroom", "Blanche's Bedroom", "Rose's Bedroom", "Sophia's Bedroom", 
            "The Kitchen", "The Bathroom",
            "The Lanai", "The Front Yard", "The Garage")
)

#How many cards?
clue_counts <- function(n_players){
  clues_per_player <- 18 %/% n_players
  
  public_clues <- 18 %% n_players
  
  return(list(clues = clues_per_player, public_clues = public_clues))
}

#FUnctions ----
clue_tracker_table <- function(i, clue_list){
  if(clue_list[i] == 0){
    rw <- paste0("<tr>", "<td style='color:#999999'> ",
                 names(clue_list)[i], "</td> <td style='color:#999999'>", 
                 round(clue_list[i]), "%", " </td></tr>")
  } else if(clue_list[i] == 100){
    rw <- paste0("<tr>", "<td style='color:#0063fb; background-color:#FFFF40; font-weight:bold'> ",
                 names(clue_list)[i], "</td> <td style='color:#FF4040'>", 
                 round(clue_list[i]), "%", " </td></tr>")
  } else if(clue_list[i] == max(clue_list, na.rm=TRUE)){
    rw <- paste0("<tr>", "<td style='color:#FF4040; font-weight:bold'> ",
                 names(clue_list)[i], "</td> <td style='color:#FF4040'>", 
                 round(clue_list[i]), "%", " </td></tr>")
  } else {
    rw <- paste0("<tr><td>", names(clue_list)[i], " <td>", round(clue_list[i]), "%", " </tr>")
  }
  return(rw)
}

calc_player_dist <- function(g_pos, d_pos, n_players, p_list){
  dist <- (d_pos - g_pos) %% n_players - 1
  
  if(g_pos == 0){
    players <- ""
  } else if(d_pos == 0){
    players <- p_list[-g_pos]
  } else if(dist > 0){
    players <- c(p_list, p_list)[(g_pos+1):(g_pos+dist)]
  } else {
    players <- ""
  }
  return(players)
}


#Used only inside Clue_Tracker
scale_to_clue <- function(df){
  dat <- df %>% 
    group_by(clue) %>% 
    mutate(.prob_scaled = ifelse(any(.prob_scaled >= 100) & .prob_scaled < 100, 0, .prob_scaled)) %>% 
    mutate(.prob_scaled = .prob_scaled / sum(.prob_scaled) * 100) %>% 
    ungroup()
  
  return(dat)
}

scale_to_player <- function(df, p_list){

  dat <- df %>% 
    #Rescale Envelope probabilities
    group_by(player, clue_type) %>% 
    mutate(.prob_scaled = ifelse(player == ".Envelope" & any(.prob_scaled >= 100) & .prob_scaled < 100, 0, .prob_scaled),
           .prob_scaled = ifelse(player == ".Envelope", .prob_scaled / sum(.prob_scaled) *100, .prob_scaled)) %>% 
    ungroup() %>% 
    #Player Rescale
    group_by(player) %>% 
    mutate(how.many.100 = sum(.prob_scaled == 100),
           what.to.scale = (clue_counts(length(p_list))$clues - how.many.100)) %>% 
    mutate(.prob_scaled = ifelse((player != ".Envelope" & player != ".PublicClues") & .prob_scaled != 100 & .prob_scaled != 0, 
                                 (.prob_scaled / (sum(.prob_scaled) - how.many.100*100)) * 100 * what.to.scale, 
                                 .prob_scaled)) %>% 
    ungroup()
}