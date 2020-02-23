# ==============================================================================
# Composition Metrics
# ==============================================================================
#' Relative Abundance
#' @description Calculate the percentage of each sample represented by the
#' specified .keep_vec or taxa.
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
#' @importFrom rlang .data
#' @export

taxa_pct <- function(.dataframe, .key_col, .counts_col,
                     .filter = NULL,
                     .unnest_col = NULL) {

  #----------------------------------------------------------------------------
  # Calculate abundance
  abund.vec <- taxa_abund(
    .dataframe = .dataframe,
    .key_col = {{ .key_col }},
    .counts_col = {{ .counts_col }},
    .filter = {{ .filter }},
    .unnest_col = {{ .unnest_col }}
  )
  #----------------------------------------------------------------------------
  prep.df <- prep_taxa_df(
    .dataframe = .dataframe,
    .key_col = {{ .key_col }},
    .unnest_col = {{ .unnest_col }},
    .filter = NULL
  )
  #----------------------------------------------------------------------------
  # Calculate the percentage of the specified .keep_vec.
  final.vec <- prep.df %>%
    dplyr::group_by({{ .key_col }}) %>%
    dplyr::summarize(total = sum({{ .counts_col }})) %>%
    original_order(.dataframe, {{ .key_col }}) %>%
    dplyr::mutate(
      abund = abund.vec,
      pct = .data$abund / .data$total * 100
    ) %>%
    original_order(.dataframe, {{ .key_col }}) %>%
    dplyr::mutate(pct = dplyr::if_else(!is.na(.data$pct),
                                       as.double(.data$pct),
                                       as.double(0))) %>%
    dplyr::pull(.data$pct)
  #----------------------------------------------------------------------------
  return(final.vec)
}
