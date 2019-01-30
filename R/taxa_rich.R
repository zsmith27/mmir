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
                      high.taxa.col = NULL, taxon = NULL,
                      exclusion.col = NULL, exclusion.vec = NULL) {
  if (nrow(long.df) < 1) return(0)
  # Prep.
  unique.id.col <- rlang::enquo(unique.id.col)
  low.taxa.col <- rlang::enquo(low.taxa.col)
  high.taxa.col <- rlang::enquo(high.taxa.col)
  exclusion.col <- rlang::enquo(exclusion.col)
  #----------------------------------------------------------------------------
  if (!is.null(taxon) &&
      nrow(dplyr::filter(long.df, (!!low.taxa.col) %in% taxon)) < 1) return(0)
  #----------------------------------------------------------------------------
  if (!rlang::quo_is_null(exclusion.col)) {
    long.df <- dplyr::filter(long.df, !(!!exclusion.col) %in% exclusion.vec)
  }
  #----------------------------------------------------------------------------
  if (is.null(taxon)) {
    final.vec <- long.df %>%
      dplyr::select(!!unique.id.col, !!low.taxa.col) %>%
      dplyr::distinct() %>%
      dplyr::count(!!unique.id.col) %>%
      original_order(long.df, !!unique.id.col) %>%
      dplyr::pull(n)
  } else {
    final.vec <- long.df %>%
      dplyr::select(!!unique.id.col,
                    !!low.taxa.col,
                    !!high.taxa.col) %>%
      dplyr::distinct() %>%
      dplyr::filter((!!low.taxa.col) %in% taxon) %>%
      dplyr::group_by(!!unique.id.col) %>%
      dplyr::count(!!unique.id.col) %>%
      original_order(long.df, !!unique.id.col) %>%
      dplyr::mutate(n = dplyr::if_else(!is.na(n), n, as.integer(0))) %>%
      dplyr::pull(n)
  }
  #----------------------------------------------------------------------------
  return(final.vec)
}
