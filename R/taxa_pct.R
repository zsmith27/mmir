#==============================================================================
# Composition Metrics
#==============================================================================
#'Percentage of a Specified .keep_vec
#'@description Calculate the percentage of each sample represented by the
#'specified .keep_vec or taxa.
#'@param .data .keep_vecomic counts arranged in a long data format.
#'@param .key_col The name of the column that contains a unique sampling
#'event ID.
#'@param .counts_col The name of the column that contains taxanomic counts.
#'@param .keep_col The name of the column that contains the taxon or taxa
#'of interest.
#'@param .keep_vec The .keep_vec or taxa of interest. To specify more than one taxa
#'use: c("TAXA1", "TAXA2", "TAXA3").
#'@return A numeric vector of percentages.
#'@export

taxa_pct <- function(.data, .key_col, .counts_col,
                     .filter = NULL,
                     .unnest_col = data) {

  #----------------------------------------------------------------------------
  # Calculate abundance
  abund.vec <- taxa_abund(.data = .data,
                          .key_col = {{.key_col}},
                          .counts_col = {{.counts_col}},
                          .filter = {{.filter}},
                          .unnest_col = {{.unnest_col}})
  #----------------------------------------------------------------------------
  prep.df <- prep_taxa_df(.data = .data,
                          .key_col = {{.key_col}},
                          .unnest_col = {{.unnest_col}},
                          .filter = NULL)
  #----------------------------------------------------------------------------
  # Calculate the percentage of the specified .keep_vec.
  final.vec <- prep.df %>%
    dplyr::group_by({{.key_col}}) %>%
    dplyr::summarize(total = sum({{.counts_col}})) %>%
    original_order(.data, {{.key_col}}) %>%
    dplyr::mutate(abund = abund.vec,
                  pct = abund / total * 100) %>%
    original_order(.data, {{.key_col}}) %>%
    dplyr::mutate(pct = dplyr::if_else(!is.na(pct), pct, as.double(0))) %>%
    dplyr::pull(pct)
  #----------------------------------------------------------------------------
  return(final.vec)
}



