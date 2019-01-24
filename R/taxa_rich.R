#'Taxon Richness
#'
#'@param long.df Taxonomic counts arranged in a long data format.
#'@param taxon.col The name of the column that contains the taxon or taxa
#'of interest.
#'@param taxon The taxon or taxa of interest. To specify more than one taxa
#'use: c("TAXA1", "TAXA2", "TAXA3").
#'@return The number of taxa identified.
#'@export

taxa_rich <- function(long.df, low.taxa.col,
                      high.taxa.col = NULL, taxon = NULL,
                      exclusion.col = NULL, exclusion.vec = NULL,
                      na.rm = TRUE) {
  if (nrow(long.df) < 1) return(0)
  # Prep.
  low.taxa.col <- rlang::enquo(low.taxa.col)
  high.taxa.col <- rlang::enquo(high.taxa.col)
  exclusion.col <- rlang::enquo(exclusion.col)


  if (!is.null(taxon) &&
      nrow(dplyr::filter(long.df, (!!low.taxa.col) %in% taxon)) < 1) return(0)
  #----------------------------------------------------------------------------
  if (!rlang::quo_is_null(exclusion.col)) {
    long.df <- dplyr::filter(long.df, !(!!exclusion.col) %in% exclusion.vec)
  }
  #----------------------------------------------------------------------------
  if (is.null(taxon)) {
    unique.vec <- long.df %>%
      dplyr::pull(!!low.taxa.col) %>%
      unique()
  } else {
    unique.vec <- long.df %>%
      dplyr::filter((!!low.taxa.col) %in% taxon) %>%
      dplyr::pull(!!high.taxa.col) %>%
      unique()
  }
  #----------------------------------------------------------------------------
  if (na.rm == TRUE) unique.vec <- na.omit(unique.vec)
  #----------------------------------------------------------------------------
  final.vec <- length(unique.vec)
  #----------------------------------------------------------------------------
  return(final.vec)
}


