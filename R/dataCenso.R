#' Last Census PEIC
#'
#' Tree census data from Ilha do Cardoso Plot.
#'
#' @format A \code{data.frame} containing the census records. It  includes
#'   the following fields:
#'   \describe{
#'     \item{quad}{Subplot or quadrat code where the individual or stem is located.}
#'     \item{tag}{Identification number or code of the tree/stem.}
#'     \item{dbh}{Diameter at breast height of the stem in millimeters (mm).}
#'     \item{ht}{Tree height in meters.}
#'     \item{dx}{X-coordinate (m) of tree location within the subplot (column axis).}
#'     \item{dy}{Y-coordinate (m) of tree location within the subplot (row axis).}
#'     \item{fam}{Botanical family.}
#'     \item{gen}{Botanical genus. If missing, extracted from \code{species}.}
#'     \item{species}{Species name (genus and specific epithet).}
#'     \item{nstem}{Number of stems for the tree; can be \code{NA} or omitted if records are per-stem.}
#'     \item{status}{Status code of the individual/stem: \code{"A"} (alive), \code{"D"} (dead), or \code{"M"} (missing).}
#'     \item{pom}{Point of measurement for DBH in meters. If missing, defaults to 1.3.}
#'     \item{date}{Date of measurement in the last census.}
#'   }
#' @source This is data collect by person("Alexandre Adalardo de", "Oliveira", , "adalardo@usp.br", role = c("aut", "cre")). Do not use for publication without proper authorization.

