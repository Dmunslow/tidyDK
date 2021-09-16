
#' Create a string of grepl functions from a vector of player names.  
#' Will create a string to find lineups that contain all players (intersection) in the vector
#' 
#'
#' @keywords internal
#'
#' @noRd

create_grep_str_intersect <- function(players){
  
  ## initialize string
  string <- paste0( 'grepl("', players[1], '", lineup_string, ignore.case = T)')
  
  if(length(players) ==1){
    
    return(string)
    
  } else if(length(players) ==2) {
    
    final_string <- paste0(string, '&grepl("', players[2], '",lineup_string, ignore.case = T)')
    return(final_string)
    
  } else {
    
    # collapse player list using string
    string_2 <- paste0(players[2:length(players)], collapse = '",lineup_string, ignore.case = T)&grepl("')
    final_string <- paste0(string, '&grepl("', string_2, '",lineup_string, ignore.case = T)')
  }
  
}


#' Create a string of grepl functions from a vector of player names.  
#' Will create a string to find lineups that contain any players in the vector (union)
#' 
#'
#' @keywords internal
#'
#' @noRd


create_grep_str_union <- function(players){
  
  ## initialize string
  string <- paste0( 'grepl("', players[1], '", lineup_string, ignore.case = T)')
  
  if(length(players) ==1){
    
    return(string)
    
  } else if(length(players) ==2) {
    
    final_string <- paste0(string, '|grepl("', players[2], '",lineup_string, ignore.case = T)')
    return(final_string)
    
  } else {
    
    # collapse player list using string
    string_2 <- paste0(players[2:length(players)], collapse = '",lineup_string, ignore.case = T)|grepl("')
    final_string <- paste0(string, '|grepl("', string_2, '",lineup_string, ignore.case = T)')
  }
  
}

