#' Abundance of a Specified Taxon
#' @description Calculate the abundance of each sample represented by the
#' specified taxon or taxa.
#' @param .data Taxonomic counts arranged in a long data format.
#' @param .key_col The name of the column that contains a unique sampling
#' event ID.
#' @param .counts_col The name of the column that contains taxanomic counts.
#' @return A numeric vector of percentages.
#' @importFrom rlang .data
#' @export



taxa_abund <- function(.dataframe, .key_col, .counts_col,
                       .filter = NULL,
                       .unnest_col = data) {
  prep.df <- prep_taxa_df(
    .data = .data,
    .key_col = {{ .key_col }},
    .unnest_col = {{ .unnest_col }},
    .filter = {{ .filter }}
  )

  # Calculate the abundance of the specified taxon.
  final.vec <- prep.df %>%
    dplyr::group_by({{ .key_col }}) %>%
    dplyr::summarise(abund = sum({{ .counts_col }})) %>%
    original_order(.data, {{ .key_col }}) %>%
    dplyr::mutate(
      abund = as.numeric(abund),
      abund = dplyr::if_else(
        !is.na(abund),
        as.numeric(abund),
        as.numeric(0)
      )
    ) %>%
    dplyr::pull(abund)
  #----------------------------------------------------------------------------
  return(final.vec)
}
# ==============================================================================
