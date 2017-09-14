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


taxa_pct_rich <- function(long.df, unique.id.col, low.taxa.col,
                          high.taxa.col, taxon = NULL, count.na = TRUE) {
  if (is.null(taxon)) stop("Must specify 'taxon'.")
  # Prep.
  unique.id.col <- rlang::enquo(unique.id.col)
  low.taxa.col <- rlang::enquo(low.taxa.col)
  high.taxa.col <- rlang::enquo(high.taxa.col)
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

  final.vec2 <- distinct.df %>%
    dplyr::mutate(rich = taxa_rich(long.df, !!unique.id.col, !!low.taxa.col, !!high.taxa.col),
                  taxa_rich = taxa_rich(long.df, !!unique.id.col, !!low.taxa.col, !!high.taxa.col, taxon),
                  pct_rich = if_else(taxa_rich == 0, 0, taxa_rich / rich * 100)) %>%
    pull(pct_rich)
  #----------------------------------------------------------------------------
  return(final.vec)
}

