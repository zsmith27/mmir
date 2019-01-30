#'Abundance of a Specified Taxon
#'@description Calculate the abundance of each sample represented by the
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

taxa_abund <- function(long.df, unique.id.col, count.col, taxon.col, taxon = NULL,
                       exclusion.col = NULL, exclusion.vec = NULL) {
  # Prep.
  unique.id.col <- enquo(unique.id.col)
  taxon.col <- enquo(taxon.col)
  count.col <- enquo(count.col)
  exclusion.col <- enquo(exclusion.col)
  #----------------------------------------------------------------------------
  if (!rlang::quo_is_null(exclusion.col)) {
    long.df <- dplyr::filter(long.df, !(!!exclusion.col) %in% exclusion.vec)
  }
  #----------------------------------------------------------------------------
  # Calculate the percentage of the specified taxon.
  if (is.null(taxon)) {
    final.vec <- long.df %>%
      dplyr::group_by(!!unique.id.col) %>%
      dplyr::summarise(abund = sum(!!count.col)) %>%
      original_order(long.df, !!unique.id.col) %>%
      dplyr::pull(abund)
  } else {
    final.vec <- long.df %>%
      dplyr::filter((!!taxon.col) %in% taxon) %>%
      dplyr::group_by(!!unique.id.col) %>%
      dplyr::summarize(abund = sum(!!count.col)) %>%
      original_order(long.df, !!unique.id.col) %>%
      dplyr::mutate(abund = as.numeric(abund),
                    abund = dplyr::if_else(!is.na(abund), abund, as.double(0))) %>%
      dplyr::pull(abund)
  }
  #----------------------------------------------------------------------------
  return(final.vec)
}
#==============================================================================
