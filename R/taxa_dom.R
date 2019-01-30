#==============================================================================
#'Percentage of the Most Dominant Taxon (Taxa)
#'
#'@param long.df Taxonomic counts arranged in a long data format.
#'@param unique.id.col The name of the column that contains a unique sampling
#'event ID.
#'@param count.col The name of the column that contains taxanomic counts.
#'@param taxon.cols The name of the column(s) that contains the taxa
#'of interest.
#'@param dom.level A numeric value, typically 1-5, the number of dominant
#' used during the calculation.
#'@return Percent of individuals that represent the most abundant taxon or taxa.
#' dom.level can be used to specify 1st-5th most abundant taxa by specifying
#' the corresponding numeric value (1-5).  Values >1 include all of the previous
#' dominance levels. For example, dom.level = 3 is the percentage of the most
#' dominant taxon, the second most dominant taxon, and the third most
#' dominant taxon. During calculation the taxa are ranked in descending order.
#' If there are two taxa with the same count at the specified dom.level, only
#' one taxon is used. Consider the following vector as an example of
#' taxonomic counts: c(10, 10, 5, 2, 2, 1). If dom.level == 1, only the first
#' "10" is selected. If dom.level == 2, the first two "10"s are selected.
#'
#' This measure is related to taxa evenness. Typically
#' degradation is associated with elevated levels of the
#' most dominant taxon (taxa); therefore, this metric typically increases
#' with degradation.
#'@export

taxa_dom <- function(long.df, unique.id.col, count.col, taxa.col, dom.level){
  unique.id.col = rlang::enquo(unique.id.col)
  count.col = rlang::enquo(count.col)
  taxa.col = rlang::enquo(taxa.col)
  #----------------------------------------------------------------------------
  final.vec <- long.df %>%
    dplyr::group_by(!!unique.id.col, !!taxa.col) %>%
    dplyr::summarize(count = sum(!!count.col)) %>%
    dplyr::group_by(!!unique.id.col) %>%
    dplyr::mutate(total = sum(count)) %>%
    dplyr::filter(dplyr::row_number(dplyr::desc(count)) <= dom.level) %>%
    dplyr::summarize(percent = sum(count) / unique(total) * 100) %>%
    original_order(long.df, !!unique.id.col) %>%
    dplyr::pull(percent)
  #----------------------------------------------------------------------------
  return(final.vec)
}
