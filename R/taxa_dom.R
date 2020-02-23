# ==============================================================================
#' Taxonomic Dominance
#' @param .dataframe A data frame where each row should represent the number of
#' individuals enumerated for a single taxon collected during a single sampling event.
#' @param .key_col One unquoted column name that represents a key (i.e., unique ID)
#'  for a sampling event for which to group (i.e., aggregate) the data.
#' @param .counts_col One unquoted column name that represents taxonomic counts.
#' @param .filter A logical statement to subset the data frame prior to calculating
#' the metric of interest.
#' @param .unnest_col One unqouted column name that represents nested data.
#'  If this column is NULL (default), then the data will not be unnested.
#' @param .group_col One unquoted column name that represents a taxomic rank
#'  or group of interest.
#' @param .dom_level A numeric value, typically 1-5, the number of dominant
#' used during the calculation.
#' @return A numeric vector. Percent of individuals that represent the most abundant taxon or taxa.
#' .dom_level can be used to specify 1st-5th most abundant taxa by specifying
#' the corresponding numeric value (1-5).  Values >1 include all of the previous
#' dominance levels. For example, .dom_level = 3 is the percentage of the most
#' dominant taxon, the second most dominant taxon, and the third most
#' dominant taxon. During calculation the taxa are ranked in descending order.
#' If there are two taxa with the same count at the specified .dom_level, only
#' one taxon is used. Consider the following vector as an example of
#' taxonomic counts: c(10, 10, 5, 2, 2, 1). If .dom_level == 1, only the first
#' "10" is selected. If .dom_level == 2, the first two "10"s are selected.
#'
#' This measure is related to taxa evenness. Typically
#' degradation is associated with elevated levels of the
#' most dominant taxon (taxa); therefore, this metric typically increases
#' with degradation.
#' @importFrom rlang .data
#' @export

taxa_dom <- function(.dataframe, .key_col, .counts_col, .group_col, .dom_level, .filter,
                     .unnest_col = NULL) {
  prep.df <- prep_taxa_df(
    .dataframe = .dataframe,
    .key_col = {{ .key_col }},
    .unnest_col = {{ .unnest_col }},
    .filter = {{ .filter }}
  )
  #----------------------------------------------------------------------------
  final.vec <- prep.df %>%
    dplyr::group_by({{ .key_col }}, {{ .group_col }}) %>%
    dplyr::summarize(count = sum({{ .counts_col }})) %>%
    dplyr::group_by({{ .key_col }}) %>%
    dplyr::mutate(total = sum(.data$count)) %>%
    dplyr::filter(dplyr::row_number(dplyr::desc(.data$count)) <= .dom_level) %>%
    dplyr::summarize(percent = sum(.data$count) / unique(.data$total) * 100) %>%
    original_order(.dataframe, {{ .key_col }}) %>%
    dplyr::pull(.data$percent)
  #----------------------------------------------------------------------------
  return(final.vec)
}
