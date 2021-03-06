% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Analysis External functions.R
\name{get_player_vect_own}
\alias{get_player_vect_own}
\title{Get Contest Ownership for multiple players in the same lineup}
\usage{
get_player_vect_own(lu_dat, lineup_players, set_type = "intersection")
}
\arguments{
\item{lu_dat}{Takes lineup data, long and tidy, or summarized and tidy.}

\item{lineup_players}{vector of player names as they appear in DKSalary file}

\item{set_type}{Which type of subset the function will return.  'intersection' 
is default and will return lineups that contain all the players in the linuep_players vector.
Alternatively, 'union' will return lineups which contain at least 1 of the players in the lineup_players vector.}
}
\value{
a subset of lu_data containing players in the lineup_players vector.
}
\description{
Returns ownership % of lineups which contain players in a vector provided by the user.
}
\examples{
\donttest{

 ## return lineups which contain both Dak Prescott and Amari Cooper
 get_player_vect_own(lu_dat, c('Dak Prescott', 'Amari Cooper'), set_type = 'intersection') 

}

}
