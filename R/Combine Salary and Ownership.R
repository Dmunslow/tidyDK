#' Combine Salary and Ownership data
#'
#' @keywords internal
#'
#' @noRd

combine_salary_own <- function(sal_clean, own_clean){

  ## note: this function combines salary and ownership data. Where possible, it
  ##       imputes the position for players in the ownership data with FLEX.  This may
  ##       not be possible when player names are duplicated in the salary file
  ##       AND both players are utilized in at least 1 lineup in the contest

  NON_FLEX_POSITIONS <- c('QB', 'P', 'G')

  ## identify dupe players
  dupe_players <- sal_clean[duplicated(player_name), unique(player_name)]

  ## part 1: non duped easy join ===============================================

  ## own no dupes
  own_no_dupes <- own_clean[!(player_name %in% dupe_players)]
  sal_no_dupes <- sal_clean[!(player_name %in% dupe_players)]

  setkey(own_no_dupes, player_name)
  setkey(sal_no_dupes, player_name)
  sal_own_1 <- merge.data.table(sal_no_dupes, own_no_dupes, all.x = T)

  ## check if there are dupes, if not just return ez join
  if(length(dupe_players) == 0 ){

      ## drop own_player_position
      drop <- 'own_position'
      return(sal_own_1[,-..drop]) ## if no dupes, return joined sal own data

      } else {


        suppressWarnings(

          dupe_summary <- sal_clean[player_name %in% dupe_players,
                                    .(n_sal_players = .N,
                                      n_pos = uniqueN(player_position),
                                      n_sal = uniqueN(player_salary),
                                      sal_diff = max(player_salary) - min(player_salary),
                                      sal_non_flex = sum(ifelse(player_position %in% NON_FLEX_POSITIONS, 1, 0))),
                                    by = player_name]
        )

        suppressWarnings(

          own_summary <- own_clean[player_name %in% dupe_players,
                                   .(n_own_players = .N,
                                     own_flex = sum(ifelse(own_position != "FLEX", 1, 0))),
                                   by = player_name]
        )

        setkey(dupe_summary, player_name)
        setkey(own_summary, player_name)
        
        
        
        decision_table <- merge.data.table(dupe_summary, own_summary, all.x = T)

        ## part 2: duped, notso easy =================================================
        own_dupes <- own_clean[player_name %in% dupe_players]
        sal_dupes <- sal_clean[player_name %in% dupe_players]

        for(i in 1:nrow(decision_table)){

          pname <- decision_table$player_name[i]

          ## if one is non flex, other is diff position but 'FLEX' own -
          if(decision_table$sal_non_flex[i] > 0 &
             decision_table$sal_non_flex[i] < decision_table$n_sal_players[i]){

            ## get imputed position
            imp_pos <- sal_clean[!(player_position %in% NON_FLEX_POSITIONS) &
                                   player_name == pname, player_position]

            ## change in place
            own_dupes[!(own_position %in% NON_FLEX_POSITIONS) &
                        player_name == pname, own_position := imp_pos]


            ## both flexable, different positions, one has position listed
          } else if (decision_table$n_pos[i] == 2 &
                     decision_table$own_flex[i] == 1) {

            ## paste name and position together, get player position for flex in own data
            imp_pos <- sal_dupes[player_name == pname &
                                   !(paste0(player_name, player_position) %in%
                                       paste0(own_dupes$player_name, own_dupes$own_position)),
                                 player_position]

            ## impute position into flex player in place
            own_dupes[player_name == pname & own_position == "FLEX", own_position := imp_pos]

          }

        } ## end for loop

        own_dupes[, player_position := own_position]

        ## join with salary, on position and name
        setkey(own_dupes, player_name, player_position)
        setkey(sal_dupes, player_name, player_position)

        sal_own_2 <- merge(sal_dupes, own_dupes, all.x = T, all.y = T)

        ## set col order for rbind
        setcolorder(sal_own_2, names(sal_own_1))
        sal_own_full <- rbind(sal_own_1, sal_own_2)

        ## get data frame for error message
        keep <- c('player_name', 'player_position', 'player_salary', 'player_team')
        
        na_players <- unique(sal_own_full[player_position == "FLEX"])
        
        na_df <- sal_own_full[player_position != "FLEX" & player_name %in% na_players, ..keep]
        
        ## thank you SO https://stackoverflow.com/questions/26083625/how-do-you-include-data-frame-output-inside-warnings-and-errors
        print_and_capture <- function(x)
        {
          paste(capture.output(print(x)), collapse = "\n")
        }

        if(nrow(sal_own_full[player_position == "FLEX"]) > 0){

          warning('There were duplicate player names in salary file which were unable to be matched to ownership and scoring data.\nPlayer position will be FLEX for those players and salary, ownership and scoring data will be all NA.
            \nPlayers with NA info: \n', print_and_capture(na_df), 
            "\n\nIf one you suspect that one of the players above is unowned,\nyou can drop the player from the salary data and re-run the cleanup function\nas demonstrated in the\ntidyDK vignette ADDLINK")

        }
        
        ## drop own_player_position
        drop <- 'own_position'
        return(sal_own_full[,-..drop])

  }

}

