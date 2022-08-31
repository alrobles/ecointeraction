#' data_example
#'
#' A dataset containing the median phylogenetic, geographical and enviromental
#' distances of birds.
#' The raw-data has the distances for each pair of birds. This is a sample
#' ready for modelling. It contains:
#' Environmental information from WorldClim project
#' Phylogenetic information from Vertlife project
#' Geografical information from GBIF project
#'
#'@docType data
#'
#'@keywords datasets
#'
#'@format A data frame with 79 rows and 5 variables:
#' \describe{
#'   \item{species}{Bird species}
#'   \item{geo_distance}{Geographical distance}
#'   \item{env_distance}{Environmental distance}
#'   \item{phylo_distance}{Phylogenetic distance}
#'   \item{incidence}{Label if a host has been reported with the interaction}
#' }
#'@example
#' data(data_example)
"data_example"
