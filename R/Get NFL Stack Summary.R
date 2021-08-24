
#' Get Lineup Stack Summary for NFL contests
#' 
#' @keywords internal
#'
#' @noRd

get_lineup_stacks_nfl <- function(tidy_proj){
  
  ## setup lineup data for stack summary =======================================
  
  ## Need to get team for qb for each lineup
  lu_qb_base <- tidy_proj[player_position == 'QB', 
                          .(qb_team = player_team, qb_opp = opp_team), 
                          by = lineup_entry_id]
  
  setkey(lu_qb_base, lineup_entry_id)
  setkey(tidy_proj, lineup_entry_id)
  tidy_proj_qb <- lu_qb_base[tidy_proj]
  
  ## for players that are on opposing team of QB, prepend 'OPP ' to positiong string
  tidy_proj_qb[player_team == qb_opp, player_position := paste0('OPP ', player_position)]
  
  ## apply to all rows
  tidy_proj_qb[player_team == qb_team, n_players_qb_stack := .N, by = lineup_entry_id]
  tidy_proj_qb[, n_players_qb_stack := max(n_players_qb_stack, na.rm = T), by = lineup_entry_id]
  
  ## for non-qb stacks, make away players OPP and home players base
  
  ## setup attributes to identify secondary stacks
  tidy_proj_qb[player_team !=qb_team & player_team != qb_opp, 
               n_players_game_non_qb := .N, by = .(lineup_entry_id, player_game)]
  
  tidy_proj_qb[player_team !=qb_team & player_team != qb_opp, 
               n_player_teams_non_qb := uniqueN(player_team), by = .(lineup_entry_id, player_game)]
  
  ## create qb stack info ======================================================
  
  ## qb team
  qb_stacks1 <- tidy_proj_qb[player_team == qb_team, 
                             .(qb_stack_str = paste(player_position, collapse = "/"),
                               qb_stack_size = .N), 
                             by = .(lineup_entry_id)
                             ][, naked_qb_ind := ifelse(qb_stack_size == 1, 1, 0)]
  ## opp team
  qb_stacks2 <- tidy_proj_qb[player_team == qb_opp, 
                             .(opp_stack_str = paste(player_position, collapse = "/"),
                               opp_stack_size = .N), 
                             by = .(lineup_entry_id)]
  
  ## left join, stacks together
  setkey(qb_stacks1, lineup_entry_id)
  setkey(qb_stacks2, lineup_entry_id)
  
  qb_stacks <- merge(qb_stacks1, qb_stacks2, all.x = T)
  
  ## clean it up
  qb_stacks[, qb_game_stack_size := (qb_stack_size + opp_stack_size) - 1]
  qb_stacks[is.na(opp_stack_str), qb_game_stack_size := qb_stack_size - 1]
  qb_stacks[!is.na(opp_stack_str), qb_stack_type := paste0(qb_stack_str, '/', opp_stack_str)]
  qb_stacks[is.na(opp_stack_str), qb_stack_type := qb_stack_str]
  qb_stacks[, qb_stack_bring_back_ind := ifelse(is.na(opp_stack_str), 0, 1)]
  
  keep <- c('lineup_entry_id', 'qb_stack_type', 'qb_game_stack_size', 'naked_qb_ind','qb_stack_bring_back_ind')
  qb_stacks <- qb_stacks[,..keep]
  
  ## secondary stack info ======================================================
  
  ## secondary game stacks *****************************************************
  
  ## need to identify secondary stacks - player game != QB game and n_players in game >1 and n_player teams > 1
  secondary_games_stacks <- tidy_proj_qb[n_players_game_non_qb > 1 & n_player_teams_non_qb > 1]
  
  ## make home team the main player, away team the 'OPP'
  secondary_games_stacks[home_away == 'AWAY', player_position := paste0('OPP ', player_position) ]
  
  sgs_1 <- secondary_games_stacks[home_away == 'HOME', 
                                  .(home_stack = paste(player_position, collapse = "/"),
                                    home_stack_size = .N),
                                  by = .(lineup_entry_id, player_game)]
  
  sgs_2 <- secondary_games_stacks[home_away == 'AWAY',  
                                  .(away_stack = paste(player_position, collapse = "/"),
                                    away_stack_size = .N),
                                  by = .(lineup_entry_id, player_game)]
  
  setkey(sgs_1, lineup_entry_id, player_game)
  setkey(sgs_2, lineup_entry_id, player_game)
  sec_game_stack <- merge(sgs_1, sgs_2, all = F)
  
  ## combine home/away into gamestack string
  sec_game_stack[, `:=`(sec_game_stack_type = paste0(home_stack, "/", away_stack))]
  
  ## some lineups have multiple secondary game stacks, collapse to 1 row/lineup_entry_id
  sec_game_stack <- sec_game_stack[ ,.(sec_game_stack_str = paste(sec_game_stack_type, collapse = ' | '),
                                       n_sec_game_stacks = .N), by = lineup_entry_id]
  
  ## secondary non-game stacks *************************************************
  
  ## need to identify secondary stacks - player game != QB game and n_players in game >1 and n_player teams > 1
  secondary_stacks <- tidy_proj_qb[n_players_game_non_qb > 1 & n_player_teams_non_qb == 1]
  
  sts <- secondary_stacks[, 
                          .(stack_str = paste(player_position, collapse = "/")),
                          by = .(lineup_entry_id, player_team)]
  
  ## some people have multiple team stacks not correlated with qb, collapse using |
  sts_w <- sts[, .(non_qb_team_stacks_str = paste(stack_str, collapse = " | "),
                   n_non_qb_team_stacks = .N), by = lineup_entry_id]
  
  ## combine all the frames ====================================================
  setkey(qb_stacks, lineup_entry_id)
  setkey(sec_game_stack, lineup_entry_id)
  setkey(sts_w, lineup_entry_id)
  
  stack_summary_final <- merge(qb_stacks, sec_game_stack, all.x = T)
  stack_summary_final <- merge(stack_summary_final, sts_w, all.x = T)
  
  ## set counts to 0 for NA stack types
  stack_summary_final[is.na(sec_game_stack_str), n_sec_game_stacks := 0]
  stack_summary_final[is.na(non_qb_team_stacks_str), n_non_qb_team_stacks := 0]
  
  return(stack_summary_final)
}