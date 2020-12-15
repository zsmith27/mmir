#' Taxonomic Richness
#' @param .dataframe A data frame where each row should represent the number of
#' individuals enumerated for a single taxon collected during a single sampling event.
#' @param .key_col One unquoted column name that represents a key (i.e., unique ID)
#'  for a sampling event for which to group (i.e., aggregate) the data.
#' @param .counts_col One unquoted column name that represents taxonomic counts.
#' zero counts are filtered out of the richness count.
#' @param .filter A logical statement to subset the data frame prior to calculating
#' the metric of interest.
#' @param .unnest_col One unqouted column name that represents nested data.
#'  If this column is NULL (default), then the data will not be unnested.
#' @param .group_col One unquoted column name that represents a taxomic rank
#'  or group of interest.
#' @return A numeric vector.
#' @importFrom rlang .data
#' @export


taxa_rich <- function(.dataframe, .key_col, .group_col, .counts_col,
                      .filter = NULL,
                      .unnest_col = NULL) {
  if (nrow(.dataframe) < 1) {
    return(0)
  }

  prep.df <- prep_taxa_df(
    .dataframe = .dataframe,
    .key_col = {{.key_col}},
    .unnest_col = {{.unnest_col}},
    .filter = {{.filter}}
  )
  #----------------------------------------------------------------------------
  final.vec <- prep.df %>%
    dplyr::filter(!is.na({{.group_col}}),
                  {{.counts_col}} != 0) %>%
    dplyr::select({{.key_col}}, {{.group_col}}) %>%
    dplyr::distinct() %>%
    dplyr::count({{.key_col}}) %>%
    original_order(
      .org_data = .dataframe,
      .key_col = {{.key_col}}
    ) %>%
    dplyr::mutate(n = dplyr::if_else(!is.na(.data$n),
                                     .data$n,
                                     as.integer(0))) %>%
    dplyr::pull(.data$n)
  #----------------------------------------------------------------------------
  return(final.vec)
}
