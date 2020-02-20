#==============================================================================
# Composition Metrics
#==============================================================================
#'Percentage of a Specified Taxon
#'@description Calculate the percentage of each sample represented by the
#'specified taxon or taxa.
#'@param .data Taxonomic counts arranged in a long data format.
#'@param .key_col The name of the column that contains a unique sampling
#'event ID.
#'@param count.col The name of the column that contains taxanomic counts.
#'@param taxon.col The name of the column that contains the taxon or taxa
#'of interest.
#'@param .keep_vec The taxon or taxa of interest. To specify more than one taxa
#'use: c("TAXA1", "TAXA2", "TAXA3").
#'@return A numeric vector of percentages.
#'@export


taxa_pct_rich <- function(.data, .key_col, .group_col,
                          .filter = NULL,
                          .unnest_col = data) {

  prep.df <- prep_taxa_df(.data = .data,
                             .key_col = {{.key_col}},
                             .unnest_col = {{.unnest_col}},
                             .filter = NULL)
  #----------------------------------------------------------------------------
  final.vec <- prep.df %>%
    dplyr::group_nest({{.key_col}}, .key = "data") %>%
    dplyr::mutate(
      rich = taxa_rich(
        .data = .,
        .key_col = {{.key_col}},
        .group_col = {{.group_col}},
        .unnest_col = data
      ),
      taxa_rich = taxa_rich(
        .data = .,
        .key_col = {{.key_col}},
        .group_col = {{.group_col}},
        .filter = {{.filter}},
        .unnest_col = data
      ),
      pct_rich = dplyr::if_else(taxa_rich == 0,
                                as.double(0),
                                as.double(taxa_rich / rich * 100))
    ) %>%
    original_order(.data, {{.key_col}}) %>%
    pull(pct_rich)
  #----------------------------------------------------------------------------
  return(final.vec)
  }
