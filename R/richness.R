#'Taxon Richness
#'
#'@param long.df Taxonomic counts arranged in a long data format.
#'@param unique.id.col The name of the column that contains a unique sampling
#'event ID.
#'@param high.res.taxa.col The name of the column that contains the taxon or taxa
#'of interest.
#'@param taxon The taxon or taxa of interest. To specify more than one taxa
#'use: c("TAXA1", "TAXA2", "TAXA3").
#'@return The number of taxa identified.
#'@export

taxa_rich <- function(long.df, unique.id.col, count.col, taxon.col,
                      taxon = NULL, high.res.taxa.col = NULL,
                      exclusion.col = NULL, exclusion.vec = NULL) {
  if (nrow(long.df) < 1) return(0)
  # Prep.
  unique.id.col <- rlang::enquo(unique.id.col)
  taxon.col <- rlang::enquo(taxon.col)
  count.col <- rlang::enquo(count.col)
  high.res.taxa.col <- rlang::enquo(high.res.taxa.col)
  if (rlang::quo_is_null(high.res.taxa.col)) high.res.taxa.col <- taxon.col
  exclusion.col <- rlang::enquo(exclusion.col)
  #----------------------------------------------------------------------------
  long.df <- long.df %>%
    dplyr::filter((!!count.col) > 0)
  #----------------------------------------------------------------------------
  if (rlang::quo_is_null(exclusion.col)) {
    # Aggregate taxonomic counts at the specified taxonomic levels.
    taxa.counts <- long.df %>%
      dplyr::select(!!unique.id.col, !!taxon.col, !!high.res.taxa.col) %>%
      dplyr::distinct()
  } else {
    taxa.counts <- long.df %>%
      dplyr::select(!!unique.id.col, !!taxon.col,
                    !!high.res.taxa.col, !!exclusion.col) %>%
      dplyr::distinct() %>%
      dplyr::filter(!rlang::UQ(exclusion.col) %in% exclusion.vec)
  }
  #----------------------------------------------------------------------------
  if (is.null(taxon)) {
    final.vec <- taxa.counts %>%
      dplyr::count(!!unique.id.col) %>%
      original_order(long.df, !!unique.id.col) %>%
      dplyr::pull(n)
  } else {
    final.vec <- taxa.counts %>%
      dplyr::group_by(!!unique.id.col, !!taxon.col) %>%
      dplyr::count(rlang::UQ(high.res.taxa.col)) %>%
      dplyr::filter(rlang::UQ(taxon.col) %in% taxon) %>%
      dplyr::group_by(!!unique.id.col) %>%
      dplyr::summarise(n = sum(n)) %>%
      original_order(long.df, !!unique.id.col) %>%
      dplyr::mutate(n = dplyr::if_else(!is.na(n), n, as.integer(0))) %>%
      dplyr::pull(n)
  }
  #----------------------------------------------------------------------------
  return(final.vec)
}
