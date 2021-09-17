#' Get Ownership for lineups from a subset of DK users
#'
#' @description Calculates player ownership for subset of lineups from DK users.
#'  Can be used to compare sharp ownership vs contest.
#'   
#' @param tidy_lus long format, tidy lineup data
#'   
#' @param dk_user_subset a vector of DK usernames - note: NOT CASE SENSITIVE
#'
#' @return a dataframe with ownership for all players in the contest, with total ownership in contest,
#'  and ownership for the subset of dk usernames provided to the function. 
#'
#' @examples
#' \donttest{
#' 
#'  own_comparison <- get_dk_user_subset_ownership(lu_dat, c('awesemo', 'youdacao', 'papagates', 'rynpak')) 
#' 
#'  own_comparison 
#' 
#' }
#'
#' @export


get_dk_user_subset_ownership <- function(tidy_lus, dk_user_subset){

  cont_own <- unique(tidy_lus[!is.na(player_name), .(player_name, player_team, player_position, player_own_pct)])

  ## subset lineups
  user_lu_sub <- tidy_lus[tolower(lineup_username) %in% tolower(dk_user_subset)]

  ## calc n lineups
  user_lu_sub[, n_subset_lineups := uniqueN(lineup_entry_id)]

  ## calc ownership
  user_own <- user_lu_sub[, .(n_lineups_with_player = .N),
                          .(player_name, n_subset_lineups)]

  user_own[, subset_player_own := round(n_lineups_with_player/n_subset_lineups, 4)]

  ## join em up
  setkey(user_own, player_name)
  setkey(cont_own, player_name)

  own_final <- user_own[cont_own]

  nlineups <- max(own_final$n_subset_lineups, na.rm = T)
  nusers <- user_lu_sub[, uniqueN(lineup_username)]

  ## for user that had 0 of a player, set value from NA to 0
  own_final[is.na(n_subset_lineups),
            `:=`(n_subset_lineups = nlineups ,
                 n_lineups_with_player = 0,
                 subset_player_own = 0)]

  own_final[, n_unique_usernames := nusers]
  own_final[, ownership_diff := subset_player_own - player_own_pct][order(-abs(ownership_diff))]

  cord <- c("player_name", "player_position", "player_team", "player_own_pct",
            "subset_player_own", "ownership_diff", "n_subset_lineups",
            "n_unique_usernames", "n_lineups_with_player")

  setcolorder(own_final, cord)

  return(own_final)

}
