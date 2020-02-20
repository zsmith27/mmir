#' Taxon Richness
#'
#' @param .data Taxonomic counts arranged in a long data format.
#' @param .key_col The name of the column that contains a unique sampling
#' event ID.
#' @param taxon.col The name of the column that contains the taxon or taxa
#' of interest.
#' @param .keep_vec The taxon or taxa of interest. To specify more than one taxa
#' use: c("TAXA1", "TAXA2", "TAXA3").
#' @return The number of taxa identified.
#' @importFrom rlang .data
#' @export


taxa_rich <- function(.data, .key_col, .group_col,
                      .filter = NULL,
                      .unnest_col = data) {
  if (nrow(.data) < 1) {
    return(0)
  }

  prep.df <- prep_taxa_df(
    .data = .data,
    .key_col = {{ .key_col }},
    .unnest_col = {{ .unnest_col }},
    .filter = {{ .filter }}
  )
  #----------------------------------------------------------------------------
  final.vec <- prep.df %>%
    dplyr::select({{ .key_col }}, {{ .group_col }}) %>%
    dplyr::distinct() %>%
    dplyr::count({{ .key_col }}) %>%
    original_order(
      .org_data = .data,
      .key_col = {{ .key_col }}
    ) %>%
    dplyr::mutate(n = dplyr::if_else(!is.na(n), n, as.integer(0))) %>%
    dplyr::pull(n)
  #----------------------------------------------------------------------------
  return(final.vec)
}
