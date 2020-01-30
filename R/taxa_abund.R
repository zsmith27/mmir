#'Abundance of a Specified Taxon
#'@description Calculate the abundance of each sample represented by the
#'specified taxon or taxa.
#'@param .data Taxonomic counts arranged in a long data format.
#'@param .key_col The name of the column that contains a unique sampling
#'event ID.
#'@param .counts_col The name of the column that contains taxanomic counts.
#'@param .filter_col The name of the column that contains the taxon or taxa
#'of interest.
#'@param .filter_vecThe taxon or taxa of interest. To specify more than one taxa
#'use: c("TAXA1", "TAXA2", "TAXA3").
#'@return A numeric vector of percentages.
#'@export


taxa_abund <- function(.data, .key_col, .counts_col,
                       .filter_col = NULL, .filter_vec= NULL,
                       .remove_col = NULL, .remove_vec = NULL,
                       .unnest_col = data) {
  .data_unnest <- tidyr::unnest(.data, cols = !!rlang::enquo(.unnest_col))

  # Prep.
  .key_col <- enquo(.key_col)
  .filter_col <- enquo(.filter_col)
  .counts_col <- enquo(.counts_col)
  .remove_col <- enquo(.remove_col)
  #----------------------------------------------------------------------------
  if (!rlang::quo_is_null(.remove_col)) {
    .data_unnest <- dplyr::filter(.data_unnest, !(!!.remove_col) %in% .remove_vec)
  }
  #----------------------------------------------------------------------------
  # Calculate the percentage of the specified taxon.
  if (is.null(.filter_vec)) {
    final.vec <- .data_unnest %>%
      dplyr::group_by(!!.key_col) %>%
      dplyr::summarise(abund = sum(!!.counts_col)) %>%
      original_order(.data, !!.key_col) %>%
      dplyr::pull(abund)
  } else {
    final.vec <- .data_unnest %>%
      dplyr::filter((!!.filter_col) %in% .filter_vec) %>%
      dplyr::group_by(!!.key_col) %>%
      dplyr::summarize(abund = sum(!!.counts_col)) %>%
      original_order(.data, !!.key_col) %>%
      dplyr::mutate(abund = as.numeric(abund),
                    abund = dplyr::if_else(!is.na(abund), abund, as.double(0))) %>%
      dplyr::pull(abund)
  }
  #----------------------------------------------------------------------------
  return(final.vec)
}
#==============================================================================
