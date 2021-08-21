#' Clean DraftKings 'contest-standings' csv Lineup Data
#'
#' @description Takes a draftkings "contest-standings" dataframe with player lineups and returns
#' a tidy version in a "long" format i.e. each row is 1 player corresponding to a lineup id.
#'
#' @param contest_data A dataframe of raw lineup data from a draftings "contest-standings" csv
#' @param sport A string specifying the sport of the contest. tidyDK currently supports
#'  'NFL', 'NHL', 'NBA' and 'MLB'
#'
#' @return The contest data for all lineups in the 'contest-standings' csv in long format.
#'  Returns a data.table object
#'
#' @examples
#' \donttest{
#'
#' lineups <- clean_lineup_data(contest_data, "NFL")
#' head(lineups)
#'
#' }
#'
#' @export


clean_lineup_data <- function(contest_data, sport){


  ## create data.table version of data
  if(!is.data.table(contest_data)){

    lineup_clean = as.data.table(contest_data)

  } else {

    lineup_clean = copy(contest_data)

  }

  ## split up the file based on the sport type
  if(sport == "NHL"){

    lineup_clean[, paste0("player_", 1:9) := tstrsplit(gsub("^\\$", "", gsub("(W |C |G |UTIL |^D | D )+", "$\\1", Lineup)), "\\$")]

  } else if (sport == "NFL"){

    lineup_clean[, paste0("player_", 1:9) := tstrsplit(gsub("^\\$", "", gsub("(QB|RB|WR|FLEX|TE|DST)+", "$\\1", Lineup)), "\\$")]

  } else if (sport == "NBA"){

    lineup_clean[, paste0("player_", 1:8) := tstrsplit(gsub("^\\$", "", gsub("(PF |PG |SG |SF |UTIL |^F | F |^G | G |^C | C )+", "$\\1", Lineup)), "\\$")]

  } else if (sport == "MLB"){

    lineup_clean[, paste0("player_", 1:10) := tstrsplit(gsub("^\\$", "", gsub("(1B|2B|3B|OF|SS|^C | C |^P | P )+", "$\\1", Lineup)), "\\$")]

  }


  ## clean contest file ========================================================

  ## make it long
  lineups_long <- melt(lineup_clean, id.vars = c("Rank", "EntryId", "EntryName",
                                                 "TimeRemaining", "Points", "Lineup"))

  ## trimws
  lineups_long[, value := trimws(value)]

  ## cleanup colum
  lineups_long[, `:=`(lineup_position = gsub("(^[^ ]+).*", "\\1", value),
                      value = trimws(gsub("^[^ ]+(.*)", "\\1", value)),
                      Points = round(Points,2),
                      EntryName = gsub("(^[^ ]+).*", "\\1", EntryName),
                      lu_pos_rank_order = as.integer(gsub('player_', '', variable)))]

  ## drop uneeded column, rename variables
  drop <- c('variable')
  cnames <- c('lineup_rank', 'lineup_entry_id', 'lineup_username', 'lineup_pmr',
              'lineup_fpts', 'lineup_string', 'player_name', 'player_lineup_position', 'lu_pos_rank_order')
  lineups_long <- lineups_long[,-..drop]
  setnames(lineups_long, cnames)

  ## add n_entries
  lineups_long[, n_entries_contest := uniqueN(lineups_long$lineup_entry_id)]

  return(lineups_long)

}

#' Clean DraftKings player information (ownership, points scored) from a
#' DraftKings'contest-standings' csv
#'
#' @description Takes a dataframe with player ownership and points information from
#' a draftkings 'contest-standings' csv, and returns a clean dataframe with values
#' converted to numeric
#'
#' @param player_contest_data A dataframe of player ownership and points scored information from a
#' DraftKings "contest-standings" csv (columns 8-11)
#'
#' @return Player information for a given contest with numeric variables
#'
#' @examples
#' \donttest{
#'
#' player_info <- clean_player_contest_info(player_data)
#' head(player_info)
#'
#' }
#' @export


## take ownership, fpts data for players in contest and clean it
clean_player_contest_info <- function(player_contest_data){

  ci_clean <- player_contest_data
  setnames(ci_clean,c("player_name","own_position", "player_own_pct", "player_fpts"))
  ci_clean[,player_own_pct := as.numeric(gsub('%', '', player_own_pct))*.01]

  return(ci_clean)

}


#' Create a Unique Slate ID
#'
#' @description Create a unique ID for a slate with the DK salary file
#'
#' @param salaries A dataframe of player ownership and points scored information from a
#' DraftKings "contest-standings" csv (columns 8-11)
#'
#' @param sport a character string with one of the sports supported by this package
#'(NFL, NBA, MLB, NHL)
#'
#' @keywords internal
#'
#' @noRd



## takes tidy salary data and creates slate id for db purposes
create_slate_id <- function(salaries, sport){

  if (!is.data.table(salaries)){
    setDT(salaries)
  }

  n_games = as.character(uniqueN(salaries[, opp_team])/2)

  n_games = ifelse(nchar(n_games) == 1, paste0("0", n_games), n_games)

  ## use earliest game date for multi-day slates
  date = gsub("-", "",as.character(salaries[, min(game_date)]))

  max_start_hour = hour(max(salaries$game_datetime))
  min_start_hour = hour(min(salaries$game_datetime))

  paste0(sport,date, n_games, min_start_hour, max_start_hour)

}



#' Clean DraftKings salary data
#'
#'
#' @description Takes a dataframe with salary data from a DraftKings'DKSalaries' csv
#' and returns a clean dataframe
#'
#'
#' @param player_data A dataframe of player ownership and points scored information from a
#' DraftKings "contest-standings" csv (columns 8-11)
#'
#' @return Player information for a given contest with numeric variables
#'
#' @examples
#' \donttest{
#'
#' salary_clean <- clean_player_contest_info(salary_data)
#' head(salary_clean)
#'
#' }
#' @export
clean_salary <- function(salary_dat, sport){

  ## create data.table version of data
  if(!is.data.table(salary_dat)){

    salary_clean = as.data.table(salary_dat)

  } else {

    salary_clean = copy(salary_dat)

  }

  salary_clean[, `:=`(home_team = gsub('(.*)@([A-Z][^ ]+).*', '\\2', `Game Info`),
                      away_team = gsub('(.*)@([A-Z][^ ]+).*', '\\1', `Game Info`))]

  salary_clean[, `:=`(opp_team = ifelse(home_team == TeamAbbrev, away_team, home_team),
                      home_away = ifelse(home_team == TeamAbbrev, "HOME", "AWAY"),
                      player_game = gsub("^(\\w+@\\w+).*", "\\1", `Game Info`),
                      n_games_slate = uniqueN(`Game Info`),
                      game_datetime = gsub('(.*)@([A-Z][^ ]+) (.*)', '\\3', `Game Info`)
  )]

  ## extrate date/datetime
  salary_clean[, `:=`(game_date = as.Date(substring(game_datetime, 1, 10),
                                          format = '%m/%d/%Y'),
                      game_datetime = as.POSIXct(substring(game_datetime, 1, 18) ,
                                                 format = '%m/%d/%Y %I:%M%p', tz = 'EST'))]

  ## drop unneeded columns,  rename columns
  drop <- c("Name + ID", "Roster Position", "home_team", "away_team", "Game Info")
  salary_clean <- salary_clean[, -..drop]
  cnames <- c("player_position", "player_name", "dk_player_id",
              "player_salary", "player_team", "avg_pts_per_game" ,
              "opp_team", "home_away","player_game","n_games_slate", "game_datetime",
              "game_date")
  setnames(salary_clean, cnames)

  ## add slate id
  slateid = create_slate_id(salary_clean, sport)

  salary_clean[, slate_id := slateid]


  ## reorder columns
  colorder <- c( "dk_player_id", "player_name",  "player_position",
                 "player_salary", "player_team", "opp_team",  "avg_pts_per_game",
                 "n_games_slate","home_away","player_game", "game_date", "slate_id"
  )

  setcolorder(salary_clean, colorder)

  return(salary_clean)
}



