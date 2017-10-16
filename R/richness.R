#'Taxon Richness
#'
#'@param long.df Taxonomic counts arranged in a long data format.
#'@param unique.id.col The name of the column that contains a unique sampling
#'event ID.
#'@param taxon.col The name of the column that contains the taxon or taxa
#'of interest.
#'@param taxon The taxon or taxa of interest. To specify more than one taxa
#'use: c("TAXA1", "TAXA2", "TAXA3").
#'@return The number of taxa identified.
#'@export

taxa_rich <- function(long.df, unique.id.col, low.taxa.col,
                      high.taxa.col, taxon = NULL) {
  # Prep.
  unique.id.col <- rlang::enquo(unique.id.col)
  low.taxa.col <- rlang::enquo(low.taxa.col)
  high.taxa.col <- rlang::enquo(high.taxa.col)
  #----------------------------------------------------------------------------

  #----------------------------------------------------------------------------
  # Aggregate taxonomic counts at the specified taxonomic levels.
  taxa.counts <- long.df %>%
    dplyr::select(!!unique.id.col, !!low.taxa.col, !!high.taxa.col) %>%
    dplyr::distinct() %>%
    dplyr::rename(UNIQUE_ID = !!unique.id.col)
  #----------------------------------------------------------------------------
  distinct.df <- taxa.counts %>%
    dplyr::select(UNIQUE_ID) %>%
    dplyr::distinct()

  if (is.null(taxon)) {
    final.vec <- taxa.counts %>%
      dplyr::count(UNIQUE_ID) %>%
      dplyr::right_join(distinct.df, by = "UNIQUE_ID") %>%
      dplyr::pull(n)
  } else {
    final.vec <- taxa.counts %>%
      dplyr::group_by(UNIQUE_ID, !!low.taxa.col) %>%
      dplyr::count(rlang::UQ(high.taxa.col)) %>%
      dplyr::filter(rlang::UQ(low.taxa.col) %in% taxon) %>%
      dplyr::group_by(UNIQUE_ID) %>%
      dplyr::summarise(n = sum(n)) %>%
      dplyr::right_join(distinct.df, by = "UNIQUE_ID") %>%
      dplyr::mutate(n = dplyr::if_else(!is.na(n), n, as.integer(0))) %>%
      dplyr::pull(n)
  }
  #----------------------------------------------------------------------------
  return(final.vec)
}
