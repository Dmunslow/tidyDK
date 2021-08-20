
#' Impute correct player for duplicated players using score in lineup
#'
#' @param duped_player_lus duped player rows
#' @param lus_minus_dupes lineups with duped player, minus duped player
#' @param sal_own_dat joined salary and ownership data
#'
#' @keywords internal
#'
#' @noRd

impute_duped_player_score <- function(duped_player_lus, lus_minus_dupes, sal_own_dat ){

  for(eid in duped_player_lus$lineup_entry_id){

    imp_player_lu <-  duped_player_lus[lineup_entry_id == eid]

    imp_player_fpts <- lus_minus_dupes[lineup_entry_id == eid,
                                       .(wo_fpts = sum(player_fpts)),
                                       by = lineup_fpts
                                       ][, round(lineup_fpts - wo_fpts, 1)]

    imp_player_ci <- sal_own_dat[player_name == imp_player_lu$player_name &
                                   player_fpts == imp_player_fpts]

    ## join em up
    setkey(imp_player_lu, player_name)
    setkey(imp_player_ci, player_name)
    imp_player_full <- imp_player_lu[imp_player_ci]

    if(exists('all_imputed')){

      all_imputed <- rbind(all_imputed, imp_player_full)

    } else {

      all_imputed <- imp_player_full

    }

  } ## end for loop

  return(all_imputed)

}
