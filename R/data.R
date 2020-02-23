#' National Rivers and Streams Assessment: Northern Appalachians Benthic Macroinvertebrates  (2008-2009)
#'
#' A dataset containing the benthic macroinvertebrates collected
#' in the Northern Appalachians as part of the 2008-2009 National
#' Rivers and Streams Assessment conducted by the United States
#' Environmental Protection Agency.
#'
#' @format A data frame with 13,609 rows and 16 variables:
#' \describe{
#'   \item{uid}{Unique site visit ID}
#'   \item{site_id}{Site identification code}
#'   \item{rt_nrsa_cat}{Impact designations for use in indicator development for NRSA}
#'   \item{target_taxon}{Taxanomic name at the target hierarchical taxonomic level}
#'   \item{total}{Total number of individuals counted}
#'   \item{phylum}{Phylum of taxon}
#'   \item{class}{Class of taxon}
#'   \item{order}{Order of taxon}
#'   \item{family}{Family of taxon}
#'   \item{subfamily}{Subfamily of taxon}
#'   \item{tribe}{Tribe of taxon}
#'   \item{genus}{Genus of taxon}
#'   \item{ffg}{Macroinvertebrate functional feeding group used in metric calculations;
#'   Collector-filterer (CF), Collector-gatherer (CG), Parasite (PA),
#'   Piercer (PI), Scraper (SC), and Shredder (SH)}
#'   \item{habit}{Habit used in metric calculations;
#'   Burrower (BU), Climber (CB), Clinger (CN), Diver (DV), Planktonic(PK),
#'   Skater (SK), Sprawler (SP), and Swimmer (SW)}
#'   \item{ptv}{Pollution tolerance value as assigned for NWSA}
#'   \item{voltinism}{Voltinism as assigned by USGS}
#'   ...
#' }
#' @source \url{https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys}
"nrsa_nap_0809"
