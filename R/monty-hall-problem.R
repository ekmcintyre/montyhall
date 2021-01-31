#'
#'
#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length of 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#'   Contestant selects a door.
#' @description
#'   'select_door()' randomly generates a numeric vector (1-3) which
#'   indicates a door produced from the create_game() function.
#'
#' @details
#'   The randomly generated numbers from this function represent either
#'   a "goat" or "car" door from the create_game() function.
#'
#' @param ... no arguments are used by the function.
#'
#' @return  The function returns a single numeric vector: 1, 2, or 3.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Host opens a goat door.
#'
#' @description
#'   'open_goat_door()' selects one of the doors the contestant did
#'   not pick.
#'
#' @details
#'   If the contestant selected a "car" door, the function randomly
#'   selects one of the two remaining doors. Both of these are "goat"
#'   doors. If the contestant selected a "goat" door, the function
#'   selects the single remaining "goat" door. The "car" door cannot
#'   be selected.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a numeric vector (1, 2, or, 3) that
#'   is different from the one returned by 'select_door()' (a.pick).
#'   The return is saved as 'opened.door'.
#'
#' @examples
#'   open_goat_door( game, a.pick)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Contestant stays or switches.
#'
#' @description
#'   'change_door()' uses the argument "stay" for the
#'   contestant deciding to stay at the door they first picked or
#'   switch to the remaining door.
#'
#' @details
#'   If the contestant decides to stay (stay=T), the numeric vector
#'   'a.pick' does not change. If the contestant decides to switch
#'   (stay=F), the numeric vector that is not 'a.pick' or
#'   'opened.door' is returned.
#'
#' @param stay   Logical argument to equal "T" or "F" to decide if the
#'               contestant stays at the a.pick door or switches.
#'
#' @return The function returns a numeric vector (1-3) that cannot
#'         match 'opened.door'.
#'
#' @examples
#'   change_door(stay=T, opened.door, a.pick)
#'   change_door(stay=F, opened.door, a.pick)
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine if the contestant is a winner.
#'
#' @description
#'   'determine_winner()' determines the contestant is a winner
#'   if their final pick is a "car" door.
#'
#' @details
#'  The function uses inputs 'final.pick' and 'game' to determine
#'  if the contestant is a winner.
#'
#' @param ... no arguments are used by the function.
#'
#' @return If the character vector for 'final.pick' is a "car" door,
#'         the function returns "WIN". If 'final.pick' is "goat",
#'         the function returns "LOSE".
#'
#' @examples
#'   determine_winner(final.pick, game)
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   Play a full Monty Hall Problem game.
#'
#' @description
#'   'play_game()' returns the game outcomes of a new Monty Hall
#'   game each time it is run.
#'
#' @details
#'   The function uses results generated by 'create_game()',
#'   'select_door()', 'open)goat_door()', 'change_door()', and
#'   'determine_winner()'. These results are printed in a table
#'   describing the outcome of two game strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return A table with the game outcome (WIN or LOSE) for each
#'         strategy (stay or switch).
#'
#' @examples
#'   play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Play game loops.
#'
#' @description
#'   'play_n_games()' will play the Monty Hall game n amount of times
#'   and return a table of game outcomes.
#'
#' @details
#'   The function will automatically run the game 100 times (n = 100).
#'   It will return a table reporting proportions of game outcomes
#'   based on two strategies. These proportions should be rounded to
#'   2 decimal places.
#'
#' @param n Set equal to any number to change how many times the game
#'          loop is played.
#'
#' @return A table with proportions of LOSE and WIN outcomes
#'         from stay and switch strategies.
#'
#' @examples
#'   play_n_games(n = 1000)
#'
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}

