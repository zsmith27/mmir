#' Taxonomic Abundance
#' @description Calculate the abundance of each sample represented by the
#' specified taxon or taxa.
#' @param .dataframe A data frame where each row should represent the number of
#' individuals enumerated for a single taxon collected during a single sampling event.
#' @param .key_col One unquoted column name that represents a key (i.e., unique ID)
#'  for a sampling event for which to group (i.e., aggregate) the data.
#' @param .counts_col One unquoted column name that represents taxonomic counts.
#' @param .filter A logical statement to subset the data frame prior to calculating
#' the metric of interest.
#' @param .unnest_col One unqouted column name that represents nested data.
#'  If this column is NULL (default), then the data will not be unnested.
#' @return A numeric vector.
#' @export

taxa_abund <- function(.dataframe, .key_col, .counts_col,
                       .filter = NULL,
                       .unnest_col = NULL) {
  prep.df <- prep_taxa_df(
    .dataframe = .dataframe,
    .key_col = {{ .key_col }},
    .unnest_col = {{ .unnest_col }},
    .filter = {{ .filter }}
  )

  # Calculate the abundance of the specified taxon.
  final.vec <- prep.df %>%
    dplyr::group_by({{ .key_col }}) %>%
    dplyr::summarise(abund = sum({{ .counts_col }})) %>%
    original_order(.dataframe, {{ .key_col }}) %>%
    dplyr::mutate(
      abund = as.numeric(.data$abund),
      abund = dplyr::if_else(
        !is.na(.data$abund),
        as.numeric(.data$abund),
        as.numeric(0)
      )
    ) %>%
    dplyr::pull(.data$abund)
  #----------------------------------------------------------------------------
  return(final.vec)
}
# ==============================================================================
