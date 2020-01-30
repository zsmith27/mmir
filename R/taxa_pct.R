#==============================================================================
# Composition Metrics
#==============================================================================
#'Percentage of a Specified .filter_vec
#'@description Calculate the percentage of each sample represented by the
#'specified .filter_vec or taxa.
#'@param .data .filter_vecomic counts arranged in a long data format.
#'@param .key_col The name of the column that contains a unique sampling
#'event ID.
#'@param .counts_col The name of the column that contains taxanomic counts.
#'@param .filter_col The name of the column that contains the taxon or taxa
#'of interest.
#'@param .filter_vec The .filter_vec or taxa of interest. To specify more than one taxa
#'use: c("TAXA1", "TAXA2", "TAXA3").
#'@return A numeric vector of percentages.
#'@export

taxa_pct <- function(.data, .key_col, .counts_col,
                     # rank.col, rank,
                     .filter_col, .filter_vec,
                     .remove_col = NULL, .remove_vec = NULL,
                     .unnest_col = data) {
  .data_unnest <- tidyr::unnest(.data, cols = !!rlang::enquo(.unnest_col))

  # Prep.
  .key_col <- enquo(.key_col)
  .filter_col <- enquo(.filter_col)
  .counts_col <- enquo(.counts_col)
  # rank.col <- enquo(rank.col)
  .remove_col <- enquo(.remove_col)
  .unnest_col <- enquo(.unnest_col)
  #----------------------------------------------------------------------------
  if (!rlang::quo_is_null(.remove_col)) {
    .data_unnest <- dplyr::filter(.data_unnest, !(!!.remove_col) %in% .remove_vec)
  }
  #----------------------------------------------------------------------------
  # Calculate abundance
  abund.vec <- taxa_abund(.data = .data,
             .key_col = !!.key_col,
             .counts_col = !!.counts_col,
             .filter_col = !!.filter_col,
             .filter_vec = .filter_vec,
             .remove_col = !!.remove_col,
             .remove_vec = .remove_vec,
             .unnest_col = !!.unnest_col)
  #----------------------------------------------------------------------------
  # Calculate the percentage of the specified .filter_vec.
  final.vec <- .data_unnest %>%
    dplyr::group_by(!!.key_col) %>%
    dplyr::summarize(total = sum(!!.counts_col)) %>%
    original_order(.data_unnest, !!.key_col) %>%
    dplyr::mutate(abund = abund.vec,
                  pct = abund / total * 100) %>%
    original_order(.data_unnest, !!.key_col) %>%
    dplyr::mutate(pct = dplyr::if_else(!is.na(pct), pct, as.double(0))) %>%
    dplyr::pull(pct)
  #----------------------------------------------------------------------------
  return(final.vec)
}



