#' Get Salary data from DB
#'
#' @description Retrieves Salary information from DK API stored in local db
#'
#' @param file_contest_id unique contest identifier used to identify draft group
#'
#'
#' @export



## takes tidy salary data and creates slate id for db purposes
get_salary_db <- function(file_contest_id, sport){

  con <- dbConnect(odbc::odbc(), "MYDK")
  
  nhl_query <- "with slate_info as (

          select distinct c.draft_group_id slate_id
          	  , starts_at 
          
          from my_dk_history.dbo.NHL_DK_API_CONTESTS c
          
          where contest_id = %s
          
          )
          
          select s.draftable_id
          	   , s.full_name player_name
          	   , s.position player_position
          	   , s.salary player_salary
          	   , s.player_team
          	   -- s.opp_team - not in data
          	   -- s.avg_pts_per_game -not in data
          	   -- s.n_games_slate - we dont need this
          	   -- s.home_away - do not need this
          	   -- s.player_game - not in data
          	   , s.slate_id
          	   , si.starts_at slate_start_dtm
          
          from my_dk_history.dbo.NHL_DK_API_SALARIES s
          	join slate_info si
          		on si.slate_id = s.slate_id"  ## parameterized, will take from sprintf
  
  
  nfl_query <- "with slate_info as (

          select distinct c.draft_group_id slate_id
          	  , starts_at 
          
          from my_dk_history.dbo.NFL_DK_API_CONTESTS c
          
          where contest_id = %s
          
          )
          
          select s.draftable_id
          	   , s.full_name player_name
          	   , s.position player_position
          	   , s.salary player_salary
          	   , s.player_team
          	   -- s.opp_team - not in data
          	   -- s.avg_pts_per_game -not in data
          	   -- s.n_games_slate - we dont need this
          	   -- s.home_away - do not need this
          	   -- s.player_game - not in data
          	   , s.slate_id
          	   , si.starts_at slate_start_dtm
          
          from my_dk_history.dbo.NFL_DK_API_SALARIES s
          	join slate_info si
          		on si.slate_id = s.slate_id" 
  
  if(sport == "NFL"){
    
    salary_data <- dbGetQuery(con, sprintf(nfl_query, file_contest_id))
  
    } else if (sport == "NHL") {
    
    salary_data <- dbGetQuery(con, sprintf(nhl_query, file_contest_id))
    
    } else {
    
      warning("INVALID SPORT ENTERED IN SALARY DB FUNCTION")
  }
  
  return(as.data.table(salary_data))
  
}

