#==============================================================================
# Composition Metrics
#==============================================================================
#'Percentage of a Specified Taxon
#'@description Calculate the percentage of each sample represented by the
#'specified taxon or taxa.
#'@param long.df Taxonomic counts arranged in a long data format.
#'@param count.col The name of the column that contains taxanomic counts.
#'@param taxon.col The name of the column that contains the taxon or taxa
#'of interest.
#'@param taxon The taxon or taxa of interest. To specify more than one taxa
#'use: c("TAXA1", "TAXA2", "TAXA3").
#'@return A numeric vector of percentages.
#'@export

taxa_pct <- function(long.df, count.col,
                     taxon.col, taxon,
                     exclusion.col = NULL, exclusion.vec = NULL) {
  # Prep.
  taxon.col <- rlang::enquo(taxon.col)
  count.col <- rlang::enquo(count.col)
  exclusion.col <- rlang::enquo(exclusion.col)
  #----------------------------------------------------------------------------
  if (rlang::quo_is_null(exclusion.col)) {
    taxa.counts <- long.df %>%
      dplyr::select(!!taxon.col, !!count.col)
  } else {
    taxa.counts <- long.df %>%
      dplyr::select(!!taxon.col,
                    !!count.col,
                    !!exclusion.col) %>%
      dplyr::filter(!(!!exclusion.col) %in% exclusion.vec)
  }
  #----------------------------------------------------------------------------
  # Calculate the percentage of the specified taxon.
  final.vec <- taxa.counts %>%
    dplyr::summarize(total = sum(!!count.col)) %>%
    dplyr::mutate(abund = taxa_abund(long.df = taxa.counts,
                             count.col = !!count.col,
                             taxon.col = !!taxon.col,
                             taxon = taxon,
                             exclusion.col = !!exclusion.col,
                             exclusion.vec = exclusion.vec),
           pct = abund / total * 100) %>%
    dplyr::mutate(pct = dplyr::if_else(!is.na(pct), pct, as.double(0))) %>%
    dplyr::pull(pct)
  #----------------------------------------------------------------------------
  return(final.vec)
}



