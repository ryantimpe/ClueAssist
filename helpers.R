#HELPERS

#Series settings ----
clue <- list(
  who = c("Dorothy", "Blanche", "Rose", "Sophia", "Stan", "Miles"),
  what = c("Bathrobe", "Feathered Slipper", "Lipstick", "Ratan Chair", "Sophia's Purse", "Whipped Cream"),
  where = c("Dorothy's Bedroom", "Blanche's Bedroom", "Rose's Bedroom", "Sophia's Bedroom", 
            "The Kitchen", "The Bathroom",
            "The Lanai", "The Front Yard", "The Garage")
)

#FUnctions ----
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

calc_player_dist <- function(g_pos, d_pos, n_players, p_list){
  dist <- (d_pos - g_pos) %% n_players - 1
  
  if(dist > 0){
    players <- c(p_list, p_list)[(g_pos+1):(g_pos+dist)]
  } else {
    players <- ""
  }
  return(players)
}