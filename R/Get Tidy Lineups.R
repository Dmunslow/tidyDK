#' Get Tidy Lineups
#'
#' @description Takes paths for draftkings contest-standings file, and corresponding salary file.
#'   returns a tidy version, where each row corresponds to a player in a lineup.
#'
#'   Note that this will result in a dataframe of length (number of players in a lineup) * (number of lineups), so
#'   for larger contests this can result in data sets with 1 million+ rows
#'
#' @param contest_path a path to the contest-standings csv you'd like to tidy up
#' @param salary_path a path to the salary file corresponding to the contest-standing file you are tidying
#' @param sport A string specifying the sport of the contest. tidyDK currently supports
#'  'NFL', 'NHL', 'NBA' and 'MLB'
#' @param contest_id an ID that you'd like to assign to all rows of the file (optional)
#'
#'
#' @return tidy data.table with a row for each player in a lineup in a contest,
#'  joined with ownership and points scored data for each player
#'
#' @examples
#' \donttest{
#' lineups <- get_tidy_lineups('./contest-standings-1234564789.csv',
#'                             './DKsalaries.csv', "NFL")
#' head(lineups)
#' }
#'
#' @export


get_tidy_lineups <- function(contest_path, salary_path, sport, contest_id = NULL){

  lineups <- fread(contest_path, select = c(1:6), fill = T, sep = ',',
                   encoding = "UTF-8", integer64 = "character") ## char for int64 to avoid dependency
  contest_info <- fread(contest_path, select = c(8:11),fill = T, sep = ',',
                        encoding = "UTF-8", integer64 = "character") ## encoding for accents and garbo
  contest_info <- contest_info[!is.na(contest_info$FPTS),]
  salary <- fread(salary_path)

  ## convert utf-8 to acii to sub non accented chars
  lineups[,Lineup := iconv(Lineup, from = "UTF8", to = 'ASCII//TRANSLIT')]
  contest_info[,Player := iconv(Player, from = "UTF8", to = 'ASCII//TRANSLIT')]

  setDT(lineups)
  setDT(contest_info)
  setDT(salary)

  ## clean data with cleaning functions
  lineups_long <- clean_lineup_data(lineups,  sport)
  ci_clean <- clean_player_contest_info(contest_info)
  salary_clean <- clean_salary(salary, sport)

  ## create sal_own
  sal_own <- combine_salary_own(salary_clean, ci_clean)

  ## drop players that are not owned in contest -- helps with dupes
  sal_own <- sal_own[!is.na(player_own_pct)]

  ## get contest_id if not supplied and its part of file name
  if(is.null(contest_id)){
    file_contest_id = gsub("(.*)[-|_]([0-9]{4,})\\.csv$", "\\2", contest_path)
  }

  ## create slate id
  file_slate_id = create_slate_id(salary_clean, sport)

  ## check for duplicate names in salary =======================================

  dupe_players <-  sal_own[duplicated(player_name), unique(player_name)]

  if(length(dupe_players) == 0){

    ## no dupes, join em up ez pz
    setkey(lineups_long, player_name)
    setkey(sal_own, player_name)
    lineups_long <- lineups_long[sal_own]

    ## add columns to lineups ====================================================

    lineups_long[, contest_id := file_contest_id]
    lineups_long[, slate_id := file_slate_id]

    lineups_long[, n_games_slate := max(n_games_slate, na.rm = T)]

    colorder <- c("slate_id","game_date", "n_games_slate", "n_entries_contest",
                  "lineup_entry_id", "lineup_username",
                  "lineup_string",  "lineup_rank", "lineup_fpts", 'lineup_pmr',
                  "player_name", 'player_lineup_position', 'player_position', "player_fpts",
                  "player_own_pct", "player_salary", "player_team", "opp_team")

    setcolorder(lineups_long, colorder)
    
    lineups_long

    return(lineups_long[order(lineup_rank, lineup_entry_id, lu_pos_rank_order)])

  } else {

    NON_FLEX_POSITIONS <- c('QB', 'P', 'G')

    msg <- paste0("At least one player name in salary data is duplicated, this can cause
             \nissues with joining. This function attempts to solve for this, but will
             \n not always be possible.
             \nDuplicated Player Names: ", paste0(dupe_players, collapse = ', '))

    dupe_summary <- sal_own[player_name %in% dupe_players,
                            .(n_sal_players = .N,
                              n_pos = uniqueN(player_position),
                              n_sal = uniqueN(player_salary),
                              sal_diff = max(player_salary) - min(player_salary),
                              non_flex = sum(ifelse(player_position %in% NON_FLEX_POSITIONS, 1, 0)),
                              diff_scores = fifelse(max(player_fpts,na.rm = T) != min(player_fpts,na.rm = T), 1, 0)
                              ),
                            by = player_name]

    warning(msg)

    ## create non-duped data =================================================

    lineup_long_sub <- lineups_long[!(player_name %in% dupe_players) | is.na(player_name)]
    salary_sub <- sal_own[!(player_name %in% dupe_players)]

    setkey(salary_sub, player_name)
    setkey(lineup_long_sub, player_name)

    lineup_long_sub <- salary_sub[lineup_long_sub]

    ## set col order for rbinds later
    lucorder <- names(lineup_long_sub)

    ## Run through Dupe logic flow ===========================================

    ##========================================================================
    ##
    ##  1 . Different positions with a non-Flex-able player
    ##
    ##

    ## separate out lineups with non-flex diff positions - easiest edge case
    nf_diff_pos <- dupe_summary[non_flex > 0 & n_pos > 1, player_name]

    if (length(nf_diff_pos) > 0){

      ## get lineup entries with duped players
      nflx_players <- lineups_long[player_name %in% nf_diff_pos & player_lineup_position %in% NON_FLEX_POSITIONS]
      flx_players <- lineups_long[player_name %in% nf_diff_pos & !(player_lineup_position %in% NON_FLEX_POSITIONS)]

      ## join players to salary data
      sal_nflx <- sal_own[player_name %in% nf_diff_pos & player_position %in% NON_FLEX_POSITIONS]
      sal_flx <- sal_own[player_name %in% nf_diff_pos & !(player_position %in% NON_FLEX_POSITIONS)]

      if(nrow(sal_flx) > 0){

        setkey(sal_flx, player_name)
        setkey(flx_players, player_name)
        flx_long <- flx_players[sal_flx]

        ## join data back into to lineup_long_sub
        setcolorder(flx_long, lucorder)
        lineup_long_sub <- rbind(lineup_long_sub, flx_long)

      }

      if(nrow(nflx_players) > 0){

        setkey(sal_nflx, player_name)
        setkey(nflx_players, player_name)
        nflx_long <- nflx_players[sal_nflx]

        ## join data back into to lineup_long_sub
        setcolorder(nflx_long, lucorder)
        lineup_long_sub <- rbind(lineup_long_sub, nflx_long)

      }

    } ## end scenario 1

    ## =====================================================================
    ##
    ##  2. Different positions, both Flex-able, not in the flex
    ##
    ##

    ## 2 positions, no non-flex
    af_diff_pos <-dupe_summary[n_pos == n_sal_players & non_flex == 0, player_name]

    if(length(af_diff_pos) > 0){

      ## easy - players not used in the flex, hard - flexed players
      not_flexed <- lineups_long[player_name %in% af_diff_pos & !grepl('FLEX|UTIL', player_lineup_position) ]
      salary_sub <- sal_own[player_name %in% af_diff_pos]

      ## join the ez ones
      if(nrow(not_flexed) > 0){

        not_flexed$player_position <- not_flexed$player_lineup_position

        setkey(not_flexed, player_name, player_position)
        setkey(salary_sub, player_name, player_position)

        not_flexed <- not_flexed[salary_sub]

        setcolorder(not_flexed, lucorder)
        lineup_long_sub <- rbind(lineup_long_sub, not_flexed)

      }

    } ## end scenario 2

    ## =====================================================================
    ##
    ##  3. Position doesnt matter, both Flex-able and placed in the flex
    ##       Must have different scores for score to be imputed
    ##
    ##

    ## get players that are duped that both can be flexed
    af <-  dupe_summary[non_flex == 0 & diff_scores == 1]

    if(nrow(af) > 0){

      ## get scores wide so we can compare scores
      af_flexed <- lineups_long[player_name %in% af$player_name & grepl('FLEX|UTIL', player_lineup_position)]

      ## get lineups for lu ids that have duped af player in FLEX, this already excludes dupe player
      af_lineups <- lineup_long_sub[lineup_entry_id %in% af_flexed$lineup_entry_id][order(lineup_entry_id)]

      ## get players with imputed scores
      af_imputed <- impute_duped_player_score(af_flexed, af_lineups, sal_own)

      setcolorder(af_imputed, lucorder)

      lineup_long_sub <- rbind(lineup_long_sub, af_imputed)

    }

    ## add columns to lineups ====================================================

    lineup_long_sub[, contest_id := file_contest_id]
    lineup_long_sub[, slate_id := file_slate_id]
    lineup_long_sub[, n_games_slate := max(n_games_slate, na.rm = T)]

    colorder <- c("slate_id","game_date", "n_games_slate", "n_entries_contest",
                  "lineup_entry_id", "lineup_username",
                  "lineup_string",  "lineup_rank", "lineup_fpts", 'lineup_pmr',
                  "player_name", 'player_lineup_position', 'player_position', "player_fpts",
                  "player_own_pct", "player_salary", "player_team", "opp_team")

    setcolorder(lineup_long_sub, colorder)

    return(lineup_long_sub[order(lineup_rank, lineup_entry_id, lu_pos_rank_order)])

  } ## end dupe check if-else

}
