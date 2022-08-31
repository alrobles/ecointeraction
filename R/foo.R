#' Pipe transforming functinos
#'
#' Like dplyr, ecointeraction also uses the pipe function, \code{\%>\%} to turn
#' function composition into a series of imperative statements.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs A visualisation and a function to apply to it
#' @examples
#' # Instead of cummulative_rate(accumulate_incidence(mammalvirus,
#' # mammal_species, incidence), id, incidence, cummulativesum)
#' # you can write
#' library(ecointeraction)
#' mammalvirus %>%
#'     accumulate_incidence(mammal_species, incidence) %>%
#'     cumulative_rate(id, incidence, cummulativesum)
NULL
