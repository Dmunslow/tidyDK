get_lineup_summary <- function(tidy_lu, analysis_type = "live", stack_summary = F){

  ## create data.table version of data
  if(!is.data.table(tidy_lu)){

    lineups = as.data.table(tidy_lu)

  } else {

    lineups = copy(tidy_lu)

  }

  ## store contest ID, add to dataframes later

  contest_id <- unique(tidy_lu$contest_id)


  ## split df into two sides, lineup (single row, and player)
  lu_keep <- c("lineup_entry_id", "lineup_username", "lineup_string", "lineup_rank",
               "lineup_fpts", "lineup_pmr", "n_entries_contest", "n_games_slate")

  player_keep <- c("lineup_entry_id", "player_name", "player_lineup_position", "player_position",
                   "player_fpts", "player_own_pct", "player_salary", "player_team")

  ## derived variables
  lu_sub <- unique(lineups[, ..lu_keep])

  ## get n dupes
  lu_sub[, n_dupes := .N - 1, by = lineup_string]

  ## calc percentiles of points and PMR
  pts_pctl <- ecdf(lu_sub$lineup_fpts)(lu_sub$lineup_fpts)
  pmr_pctl <- ecdf(lu_sub$lineup_pmr)(lu_sub$lineup_pmr)

  lu_sub[, `:=`(lu_pts_pctl =  pts_pctl,
                lu_pmr_pctl = pmr_pctl)]

  ### calc player level stats ==================================================

  player_sub <- lineups[,..player_keep]


  if(analysis_type == "live"){

    if(stack_summary == T){

      warning("Stack Summary Not Available for Live Contests")

    }

    ## get remaining player positions
    keep_locks <- c("lineup_entry_id", "player_name", "player_lineup_position")

    locked_players <- player_sub[player_name == "LOCKED",..keep_locks]

    locked_players[, player_name := paste0(player_name, seq_len(.N)), by = lineup_entry_id]

    if (nrow(locked_players) > 0){

      locked_wide <- dcast(locked_players, lineup_entry_id ~ player_name,
                           value.var = "player_lineup_position")

      ## create players rem var
      locked_wide[, player_pos_remaining := do.call(paste, c(.SD, sep = "/")), .SDcols = !c("lineup_entry_id")]
      locked_wide[, player_pos_remaining := gsub("/NA", "", player_pos_remaining)] ## remove NAs

      keep_lock <- c("lineup_entry_id", "player_pos_remaining")

      locked_wide <- locked_wide[, ..keep_lock]
    }

    ### calc summary
    suppressWarnings(
      ply_sum <- player_sub[,.(salary_spent = sum(player_salary, na.rm = T),
                               salary_remaining = 50000 - sum(player_salary, na.rm = T),
                               pct_sal_remaining = round((50000 - sum(player_salary, na.rm = T)) / 50000,2),
                               min_player_sal = as.integer(min(player_salary, na.rm = T)),
                               max_player_sal = as.integer(max(player_salary, na.rm = T)),

                               players_remaining = sum(fifelse(player_name == "LOCKED", 1 ,0)),
                               cumulative_own = sum(player_own_pct, na.rm = T)
      ),
      by = lineup_entry_id]
    )


    ### join player and lu stat
    setkey(ply_sum, lineup_entry_id)
    setkey(lu_sub, lineup_entry_id)

    sum_full <- lu_sub[ply_sum]

    if (nrow(locked_players) > 0){

      setkey(locked_wide, lineup_entry_id)

      sum_full <- locked_wide[sum_full]

      ## rearrange cols
      neworder <- c("lineup_entry_id", "lineup_username", "lineup_string", "lineup_rank",
                    "lineup_fpts", "lineup_pmr","n_entries_contest", "n_dupes", "lu_pts_pctl",
                    "lu_pmr_pctl", "salary_spent", "salary_remaining", "pct_sal_remaining",
                    "min_player_sal", "max_player_sal", "players_remaining" ,
                    "player_pos_remaining", "cumulative_own", "n_games_slate" )

    } else {

      ## rearrange cols
      neworder <- c("lineup_entry_id", "lineup_username", "lineup_string", "lineup_rank",
                    "lineup_fpts", "lineup_pmr","n_entries_contest", "n_dupes", "lu_pts_pctl",
                    "lu_pmr_pctl", "salary_spent", "salary_remaining", "pct_sal_remaining",
                    "min_player_sal", "max_player_sal", "players_remaining" ,
                    "cumulative_own", "n_games_slate" )

    }

    setcolorder(sum_full, neworder = neworder)

    ## add contest id
    sum_full[, contest_id := rep(contest_id, nrow(sum_full))]

    return(sum_full)

  } else if(analysis_type == "post"){

    suppressWarnings(
    ## calc play sum
    ply_sum <-  player_sub[, .(salary_spent = sum(player_salary, na.rm = T),
                               cumulative_own = sum(player_own_pct, na.rm = T),
                               min_player_sal = as.integer(min(player_salary, na.rm = T)),
                               max_player_sal = as.integer(max(player_salary, na.rm = T)),
                               n_lte_1_pct_own = sum( fifelse(player_own_pct  <= .01, 1, 0), na.rm = T),
                               n_lte_5_pct_own = sum( fifelse(player_own_pct  <= .05, 1, 0), na.rm = T),
                               n_lte_10_pct_own = sum(fifelse(player_own_pct <= .10, 1, 0) , na.rm = T),
                               n_lte_20_pct_own = sum(fifelse(player_own_pct <= .20, 1, 0) , na.rm = T),
                               n_lte_30_pct_own = sum(fifelse(player_own_pct <= .30, 1, 0) , na.rm = T),
                               n_lte_40_pct_own = sum(fifelse(player_own_pct <= .40, 1, 0) , na.rm = T),
                               n_lte_50_pct_own = sum(fifelse(player_own_pct <= .50, 1, 0) , na.rm = T)),
                           by = lineup_entry_id]
    )

    ### join player and lu stat
    setkey(ply_sum, lineup_entry_id)
    setkey(lu_sub, lineup_entry_id)

    sum_full <- lu_sub[ply_sum]

    ## add contest id
    sum_full[, contest_id := rep(contest_id, nrow(sum_full))]

    ## drop pmr column for post analysis
    post_drop <- c("lu_pmr_pctl", "lineup_pmr")


    if(stack_summary == T){

      stacks <- get_lineup_stacks_nhl(tidy_lu)

      ## join and return
      setkey(stacks, lineup_entry_id)
      setkey(sum_full, lineup_entry_id)

      stack_lu_sum_full <- stacks[sum_full]

      ## set col order, push stack data to the back
      setcolorder(stack_lu_sum_full, names(sum_full))

      return(stack_lu_sum_full)

    } else {

      return(sum_full[, -..post_drop])
    }



  } else if (analysis_type =="full") {

    ## get remaining player positions
    keep_locks <- c("lineup_entry_id", "player_name", "player_lineup_position")

    locked_players <- player_sub[player_name == "LOCKED",..keep_locks]

    locked_players[, player_name := paste0(player_name, seq_len(.N)), by = lineup_entry_id]

    locked_wide <- dcast(locked_players, lineup_entry_id ~ player_name,
                         value.var = "player_lineup_position")

    ## create players rem var
    locked_wide[, player_pos_remaining := do.call(paste, c(.SD, sep = "/")), .SDcols = !c("lineup_entry_id")]
    locked_wide[, player_pos_remaining := gsub("/NA", "", player_pos_remaining)] ## remove NAs

    keep_lock <- c("lineup_entry_id", "player_pos_remaining")

    locked_wide <- locked_wide[, ..keep_lock]


    ## calc play sum
    ply_sum <-  player_sub[, .(salary_spent = sum(player_salary, na.rm = T),
                               salary_remaining = 50000 - sum(player_salary, na.rm = T),
                               pct_sal_remaining = round((50000 - sum(player_salary, na.rm = T)) / 50000,2),

                               players_remaining = sum(fifelse(player_name == "LOCKED", 1 ,0)),
                               cumulative_own = sum(player_own_pct, na.rm = T),
                               n_lte_1_pct_own = sum( fifelse(player_own_pct  <= .01, 1, 0), na.rm = T),
                               n_lte_5_pct_own = sum( fifelse(player_own_pct  <= .05, 1, 0), na.rm = T),
                               n_lte_10_pct_own = sum(fifelse(player_own_pct <= .10, 1, 0) , na.rm = T),
                               n_lte_20_pct_own = sum(fifelse(player_own_pct <= .20, 1, 0) , na.rm = T),
                               n_lte_30_pct_own = sum(fifelse(player_own_pct <= .30, 1, 0) , na.rm = T),
                               n_lte_40_pct_own = sum(fifelse(player_own_pct <= .40, 1, 0) , na.rm = T),
                               n_lte_50_pct_own = sum(fifelse(player_own_pct <= .50, 1, 0) , na.rm = T)),
                           by = lineup_entry_id]

    ### join player and lu stat
    setkey(ply_sum, lineup_entry_id)
    setkey(lu_sub, lineup_entry_id)
    setkey(locked_wide, lineup_entry_id)

    sum_full <- lu_sub[ply_sum]

    sum_full <- locked_wide[sum_full]

    ## add contest id
    sum_full[, contest_id := rep(contest_id, nrow(sum_full))]

    return(sum_full)

  } else {

    warning("analysis type not reconized")
  }




}
