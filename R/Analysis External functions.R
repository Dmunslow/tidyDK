
#' Get Player Lineups
#'
#' @description Returns subset of lineups which contain players provided to function.
#'  User specifies the set type, 'intersect' will return lineups that contain all players
#'  union will return lineups that contain any player in the provided vector.
#'   
#' @param lu_dat Takes lineup data, long and tidy, or summarized and tidy. 
#'   
#' @param lineup_players vector of player names as they appear in DKSalary file
#' @param set_type Which type of subset the function will return.  'intersection' 
#' is default and will return lineups that contain all the players in the linuep_players vector.
#' Union will return lineups which contain at least 1 of the players in the lineup_players vector.
#'
#' @return a subset of lu_data containing players in the lineup_players vector.
#'
#' @examples
#' \donttest{
#' 
#'  ## return lineups which contain both Dak Prescott and Amari Cooper
#'  dak_amari <- get_player_lineups(lu_dat, c('Dak Prescott', 'Amari Cooper'), set_type = 'intersection') 
#' 
#' }
#'
#' @export

get_player_lineups <- function(lu_dat, lineup_players, set_type = 'intersection'){
  
  if(!('lineup_string' %in% names(lu_dat))){
    
    stop("Linuep data does not contain column 'lineup_string'.  If you are using raw csv data, rename the 'Lineup' column to 
         'lineup_string' and retry.")
  }
  
  if(set_type == 'intersection'){
    ## get grep string
    grep_string <- create_grep_str_intersect(lineup_players)
  } else if (set_type == 'union'){
    
    grep_string <- create_grep_str_union(lineup_layers)
  } else {
    
    stop("Set Type not recognized, choose intersection for lineups that contain all players in a vector 
         or union for lineups that contain any player in the vector of player names.")
  }
  
  ## create subset string
  subset_string <- paste0("lu_dat[", grep_string, "]")
  
  lu_subset <- eval(parse(text = subset_string))
  
  return(lu_subset)
  
}


#' Get Contest Ownership for multiple players in the same lineup 
#'
#' @description Returns ownership % of lineups which contain players in a vector provided by the user.
#'   
#' @param lu_dat Takes lineup data, long and tidy, or summarized and tidy. 
#'   
#' @param lineup_players vector of player names as they appear in DKSalary file
#' @param set_type Which type of subset the function will return.  'intersection' 
#' is default and will return lineups that contain all the players in the linuep_players vector.
#' Union will return lineups which contain at least 1 of the players in the lineup_players vector.
#'
#' @return a subset of lu_data containing players in the lineup_players vector.
#'
#' @examples
#' \donttest{
#' 
#'  ## return lineups which contain both Dak Prescott and Amari Cooper
#'  get_player_vect_own(lu_dat, c('Dak Prescott', 'Amari Cooper'), set_type = 'intersection') 
#' 
#' }
#'
#' @export

get_player_vect_own <- function(lu_dat, lineup_players, set_type = 'intersection'){
  
  
  if(set_type == 'intersection'){
    ## get grep string
    grep_string <- create_grep_str_intersect(lineup_players)
  } else if (set_type == 'union'){
    
    grep_string <- create_grep_str_union(lineup_layers)
  } else {
    
    stop("Set Type not recognized, choose intersection for lineups that contain all players in a vector 
         or union for lineups that contain any player in the vector of player names.")
  }
  
  ## create subset string
  subset_string <- paste0("lu_dat[", grep_string, "]")
  
  lu_subset <- eval(parse(text = subset_string))
  
  own_pct <- round( uniqueN(lu_subset$lineup_entry_id) / uniqueN(lu_dat$lineup_entry_id), 4)
  
  sus
  return(own_pct)
  
}

