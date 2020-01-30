#'Taxon Richness
#'
#'@param .data Taxonomic counts arranged in a long data format.
#'@param .key_col The name of the column that contains a unique sampling
#'event ID.
#'@param taxon.col The name of the column that contains the taxon or taxa
#'of interest.
#'@param .filter_vec The taxon or taxa of interest. To specify more than one taxa
#'use: c("TAXA1", "TAXA2", "TAXA3").
#'@return The number of taxa identified.
#'@export


taxa_rich <- function(.data, .key_col, .group_col,
                      .filter_col = NULL, .filter_vec = NULL,
                      .remove_col = NULL, .remove_vec = NULL,
                      .unnest_col = data) {
  .data <- tidyr::unnest(.data, cols = !!rlang::enquo(.unnest_col))
  if (nrow(.data) < 1) return(0)
  # Prep.
  .key_col <- rlang::enquo(.key_col)
  .group_col <- rlang::enquo(.group_col)
  .filter_col <- rlang::enquo(.filter_col)
  .remove_col <- rlang::enquo(.remove_col)
  #----------------------------------------------------------------------------
  if (!is.null(.filter_vec) &&
      nrow(dplyr::filter(.data, (!!.filter_col) %in% .filter_vec)) < 1) return(0)
  #----------------------------------------------------------------------------
  if (!rlang::quo_is_null(.remove_col)) {
    .data <- dplyr::filter(.data, !(!!.remove_col) %in% .remove_vec)
  }
  #----------------------------------------------------------------------------
  if (is.null(.filter_vec)) {
    final.vec <- .data %>%
      dplyr::select(!!.key_col, !!.group_col) %>%
      dplyr::distinct() %>%
      dplyr::count(!!.key_col) %>%
      original_order(.data, !!.key_col) %>%
      dplyr::pull(n)
  } else {
    final.vec <- .data %>%
      dplyr::select(!!.key_col,
                    !!.group_col,
                    !!.filter_col) %>%
      dplyr::distinct() %>%
      dplyr::filter((!!.filter_col) %in% .filter_vec) %>%
      dplyr::group_by(!!.key_col) %>%
      dplyr::count(!!.key_col) %>%
      original_order(.data, !!.key_col) %>%
      dplyr::mutate(n = dplyr::if_else(!is.na(n), n, as.integer(0))) %>%
      dplyr::pull(n)
  }
  #----------------------------------------------------------------------------
  return(final.vec)
}
