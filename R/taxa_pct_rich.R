# ==============================================================================
# Composition Metrics
# ==============================================================================
#' Relative Taxonomic Richness
#' @description Calculate the percentage of each sample represented by the
#' specified taxon or taxa.
#' @param .dataframe A data frame where each row should represent the number of
#' individuals enumerated for a single taxon collected during a single sampling event.
#' @param .key_col One unquoted column name that represents a key (i.e., unique ID)
#'  for a sampling event for which to group (i.e., aggregate) the data.
#' @param .counts_col One unquoted column name that represents taxonomic counts.
#' Zero counts will be excluded from the calculation.
#' @param .filter A logical statement to subset the data frame prior to calculating
#' the metric of interest.
#' @param .unnest_col One unqouted column name that represents nested data.
#'  If this column is NULL (default), then the data will not be unnested.
#' @param .group_col One unquoted column name that represents a taxomic rank
#'  or group of interest.
#' @return A numeric vector.
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @export


taxa_pct_rich <- function(.dataframe, .key_col, .group_col, .counts_col,
                          .filter = NULL,
                          .unnest_col = NULL) {
  prep.df <- prep_taxa_df(
    .dataframe = .dataframe,
    .key_col = {{.key_col}},
    .unnest_col = {{.unnest_col}},
    .filter = NULL
  )
  #----------------------------------------------------------------------------
  group.df <- prep.df %>%
    dplyr::group_nest({{.key_col}}, .key = "data")

  final.vec <- group.df %>%
    dplyr::mutate(
      rich = taxa_rich(
        .dataframe = group.df,
        .key_col = {{.key_col}},
        .group_col = {{.group_col}},
        .counts_col = {{.counts_col}},
        .unnest_col = .data$data
      ),
      taxa_rich = taxa_rich(
        .dataframe = group.df,
        .key_col = {{.key_col}},
        .group_col = {{.group_col}},
        .counts_col = {{.counts_col}},
        .filter = {{.filter}},
        .unnest_col = .data$data
      ),
      pct_rich = dplyr::if_else(
        .data$taxa_rich == 0,
        as.double(0),
        as.double(.data$taxa_rich / .data$rich * 100)
      )
    ) %>%
    original_order(.dataframe, {{.key_col}}) %>%
    dplyr::pull(.data$pct_rich)
  #----------------------------------------------------------------------------
  return(final.vec)
}
