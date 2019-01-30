#==============================================================================
# Composition Metrics
#==============================================================================
#'Percentage of a Specified Taxon
#'@description Calculate the percentage of each sample represented by the
#'specified taxon or taxa.
#'@param long.df Taxonomic counts arranged in a long data format.
#'@param unique.id.col The name of the column that contains a unique sampling
#'event ID.
#'@param count.col The name of the column that contains taxanomic counts.
#'@param taxon.col The name of the column that contains the taxon or taxa
#'of interest.
#'@param taxon The taxon or taxa of interest. To specify more than one taxa
#'use: c("TAXA1", "TAXA2", "TAXA3").
#'@return A numeric vector of percentages.
#'@export

taxa_pct <- function(long.df, unique.id.col, count.col,
                     # rank.col, rank,
                     taxon.col, taxon,
                     exclusion.col = NULL, exclusion.vec = NULL) {
  # Prep.
  unique.id.col <- enquo(unique.id.col)
  taxon.col <- enquo(taxon.col)
  count.col <- enquo(count.col)
  # rank.col <- enquo(rank.col)
  exclusion.col <- enquo(exclusion.col)
  #----------------------------------------------------------------------------
  if (!rlang::quo_is_null(exclusion.col)) {
    long.df <- dplyr::filter(long.df, !(!!exclusion.col) %in% exclusion.vec)
  }
  #----------------------------------------------------------------------------
  # Calculate the percentage of the specified taxon.
  final.vec <- taxa.counts %>%
    dplyr::group_by(!!unique.id.col) %>%
    dplyr::summarize(total = sum(!!count.col)) %>%
    original_order(long.df, !!unique.id.col) %>%
    dplyr::mutate(abund = taxa_abund(long.df = long.df,
                                     count.col = !!count.col,
                                     taxon.col = !!taxon.col,
                                     taxon = taxon,
                                     exclusion.col = !!exclusion.col,
                                     exclusion.vec = exclusion.vec),
                  pct = abund / total * 100) %>%
    original_order(long.df, !!unique.id.col) %>%
    dplyr::mutate(pct = dplyr::if_else(!is.na(pct), pct, as.double(0))) %>%
    dplyr::pull(pct)
  #----------------------------------------------------------------------------
  return(final.vec)
}



