#==============================================================================
#'Percentage of the Most Dominant Taxon (Taxa)
#'
#'@param .data Taxonomic counts arranged in a long data format.
#'@param .key_col The name of the column that contains a unique sampling
#'event ID.
#'@param .counts_col The name of the column that contains taxanomic counts.
#'@param .group_cols The name of the column(s) that contains the taxa
#'of interest.
#'@param .dom_level A numeric value, typically 1-5, the number of dominant
#' used during the calculation.
#'@return Percent of individuals that represent the most abundant taxon or taxa.
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
#'@importFrom rlang .data
#'@export

taxa_dom <- function(.data, .key_col, .counts_col, .group_col, .dom_level, .filter,
                     .unnest_col = data){

  prep.df <- prep_taxa_df(.data = .data,
                          .key_col = {{.key_col}},
                          .unnest_col = {{.unnest_col}},
                          .filter = {{.filter}})
  #----------------------------------------------------------------------------
  final.vec <- prep.df %>%
    dplyr::group_by({{.key_col}}, {{.group_col}}) %>%
    dplyr::summarize(count = sum({{.counts_col}})) %>%
    dplyr::group_by({{.key_col}}) %>%
    dplyr::mutate(total = sum(count)) %>%
    dplyr::filter(dplyr::row_number(dplyr::desc(count)) <= .dom_level) %>%
    dplyr::summarize(percent = sum(count) / unique(total) * 100) %>%
    original_order(.data, {{.key_col}}) %>%
    dplyr::pull(percent)
  #----------------------------------------------------------------------------
  return(final.vec)
}
