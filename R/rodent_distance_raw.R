#' rodent_distance_raw
#' A data set containing the  phylogenetic geometric mean,
#' median geographical distance and most probable environmental distances of rodents.
#' Environmental information from WorldClim project (PCA of bio layers)
#' The raw-data has the distances for each pair of rodents.
#' Phylogenetic information from Nathan Upham project,
#' Geografical information from UICN project.
#'
#'@docType data
#'
#'@keywords datasets
#'
#'@format A data frame with 1191 rows and 12 variables:
#' \describe{
#'   \item{species}{Rodent species}
#'   \item{geo_distance}{Geographical distance}
#'   \item{env_distance}{Environmental distance}
#'   \item{phy_distance}{Phylogenetic distance}
#' }
#'@examples
#' data(rodent_distance_raw)
"rodent_distance_raw"
