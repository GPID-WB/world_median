

#' implicit poverty line
#'
#' @param goal numeric: population share to get implicit line for. Must be
#'   between 0 and 1. Default is .5
#' @param pl numeric: initial poverty line. Default is 2.15
#' @param tolerance numeric: decimal precission. Default is 5 decimal places
#'   decimals
#' @param ni numeric: number of iterations before converging. Default 40.
#' @param first_delta numeric: first jump. Default 3
#' @inheritDotParams pipapi::pip
#'
#' @return numeric value
#' @export
#'
#' @examples
implicit_povline <- function(goal            = 0.5,
                             povline         =  2.15,
                             country         = "AGO",
                             year            = 1987,
                             reporting_level = "urban",
                             fill_gaps       = TRUE,
                             welfare_type    = "all",
                             lkup,
                             complete_return = FALSE,
                             tolerance       = 4,
                             ni              = 40,
                             first_delta     = 3
) {

  # initial parameters -----------

  s          <- 0    # iteration stage counter
  num        <- 1    # numerator
  i          <- 1    # general counter
  status     <- "OK"


  #   main calculations ----------
  ## recording vectors --------------

  attempt <- delta <- pl <- vector("double", length = ni)
  delta[i] <- first_delta
  pl[i]    <- povline


  ## First call ---------
  # cli::cli_progress_bar(format = "{country}-{year}")
  attempt[i] <- pip_call(povline = pl[i],
                         lkup    = lkup,
                         country = country,
                         year    = year)

  # attempt <- pip_call(povline = pl,
  #                     country = "AGO",
  #                     year    = 1987,
  #                     fill_gaps = TRUE,
  #                     reporting_level = "urban",
  #                     lkup = lkup)

  ## in case there is no data for requested year---------

  if (length(attempt[i]) == 0) {
    s          <- ni + 1 # avoid the while loop
    attempt[i] <- 0
    goal       <-  NA
    pl[i]      <-  NA
    status     <- "No data"
  }


  #   start looping -------------


  while (!identical(round(attempt[i],tolerance), goal) && i < ni) {
    i <-  i + 1

    jump <- delta[i - 1]
    if (attempt[i - 1] < goal) {
      # before crossing goal
      while (pl[i - 1] + jump < 0) {
        jump <- jump * 2
      }
      pl[i] <- pl[i - 1] + jump
      below <- 1
    }

    if (attempt[i - 1] > goal) {
      # after crossing goal
      while (pl[i - 1] - jump < 0) {
        jump <- jump / 2
      }
      pl[i] <- pl[i - 1] - jump
      below <-  0
    }


    # call data
    attempt[i] <- pip_call(povline = pl[i],
                           lkup    = lkup,
                           country = country,
                           year    = year)

    # assess if the value of delta has to change
    if ((attempt[i] > goal & below == 1) |
        (attempt[i] < goal & below == 0)) {
      s <- s + 1

      if (!identical(s %% 2, 0)) {
        one <- -1
      } else {
        one <-  1
      }

      num <- (2 * num) + one
      den <- 2 ^ s
      delta[i] <- (num / den) * jump

    } else {
      delta[i] <- jump
    }  # end of condition to change the value of delta


    # cli::cli_progress_update()
  }  # end of while
  # cli::cli_progress_update(force = TRUE)

  if (complete_return) {
    list(attempt = attempt[1:i],
         delta   = delta[1:i],
         povline = pl[1:i],
         final   = pl[i],
         iterations = i)
  } else {
    return(pl[i])
  }


}  # End of function povcalnet_iterate


pip_call <- function(povline, lkup, ...) {
  pipapi::pip_grp_logic(povline = povline,
              lkup    = lkup,
              ...) |>
    fselect(headcount) |>
    reg_elem()
}
