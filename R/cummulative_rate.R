#' cummulative_rate
#' @importFrom dplyr mutate lag
#' @importFrom rlang enquo
#' @param data A \code{data.frame} object with interaction information
#' @param id Row id of incidence
#' @param incidence The incidence of interaction in the data \code{data.frame}
#' @param cummulativesum cummulative counted incidences
#'
#' @return A \code{data.frame} with the rate of change from fractional cuumlative incidence
#' @export
#'
#' @examples
#' library(ecointeraction)
#' data <-  acummulate_incidence(mammalvirus, mammal_species, incidence)
#' cumrateData <- cummulative_rate(data, id, incidence, cummulativesum)
cummulative_rate <- function(data, id, incidence, cummulativesum){
  id <- rlang::enquo(id)
  incidence <- rlang::enquo(incidence)
  cummulativesum <- rlang::enquo(cummulativesum)

  data %>%
    dplyr::mutate(cummulativesum_frac = !! cummulativesum/sum(!! incidence)) %>%
    dplyr::mutate(id_fraction = !! id/max(!! id) ) %>%
    dplyr::mutate(rate = id_fraction/cummulativesum_frac ) %>%
    dplyr::mutate(ratediff = log(rate - dplyr::lag(rate)))  %>%
    dplyr::mutate(ratediffdiff = round(ratediff - dplyr::lag(ratediff), 3)^2 )
}

