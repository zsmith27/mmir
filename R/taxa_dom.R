#==============================================================================
#'Percentage of the Most Dominant Taxon (Taxa)
#'
#'@param .data Taxonomic counts arranged in a long data format.
#'@param .key_col The name of the column that contains a unique sampling
#'event ID.
#'@param .count_col The name of the column that contains taxanomic counts.
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
#'@export

<<<<<<< HEAD
taxa_dom <- function(.data, .key_col, .count_col, .group_col, .dom_level,
                     .unnest_col = data){
  .data <- tidyr::unnest(.data, cols = !!rlang::enquo(.unnest_col))
  .key_col = rlang::enquo(.key_col)
  .count_col = rlang::enquo(.count_col)
  .group_col = rlang::enquo(.group_col)
=======
taxa_dom <- function(long.df, unique.id.col, count.col, taxon.col, dom.level,
                     unnest.cols = data){
  long.df <- tidyr::unnest(long.df, cols = !!rlang::enquo(unnest.cols))
  unique.id.col = rlang::enquo(unique.id.col)
  count.col = rlang::enquo(count.col)
  taxon.col = rlang::enquo(taxon.col)
>>>>>>> 639a17f21c077953610adaba81369fd2ce5557f1
  #----------------------------------------------------------------------------
  final.vec <- .data %>%
    dplyr::group_by(!!.key_col, !!.group_col) %>%
    dplyr::summarize(count = sum(!!.count_col)) %>%
    dplyr::group_by(!!.key_col) %>%
    dplyr::mutate(total = sum(count)) %>%
    dplyr::filter(dplyr::row_number(dplyr::desc(count)) <= .dom_level) %>%
    dplyr::summarize(percent = sum(count) / unique(total) * 100) %>%
    original_order(.data, !!.key_col) %>%
    dplyr::pull(percent)
  #----------------------------------------------------------------------------
  return(final.vec)
}
