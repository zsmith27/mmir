#==============================================================================
# Composition Metrics
#==============================================================================
#'Percentage of a Specified Taxon
#'@description Calculate the percentage of each sample represented by the
#'specified taxon or taxa.
#'@param long.df Taxonomic counts arranged in a long data format.
#'@param unique.id.col The name of the column that contains a unique sampling
#'event ID.
#'@param count.col The name of the column that contains taxanomic counts.
#'@param taxon.col The name of the column that contains the taxon or taxa
#'of interest.
#'@param taxon The taxon or taxa of interest. To specify more than one taxa
#'use: c("TAXA1", "TAXA2", "TAXA3").
#'@return A numeric vector of percentages.
#'@export

taxa_tol_index <- function(long.df, unique.id.col, count.col, taxon.col, tol.col) {
  # Prep.
  unique.id.col <- enquo(unique.id.col)
  count.col <- enquo(count.col)
  taxon.col <- enquo(taxon.col)
  tol.col <- enquo(tol.col)
  #----------------------------------------------------------------------------
  taxa.count <- long.df %>%
    dplyr::select(!!unique.id.col, !!count.col, !!taxon.col) %>%
    dplyr::group_by(!!unique.id.col, !!taxon.col) %>%
    dplyr::summarize(counts = sum(!!count.col))

  tol.df <- long.df %>%
    dplyr::select(!!taxon.col, !!tol.col) %>%
    dplyr::distinct()

  join.df <- dplyr::left_join(taxa.count, tol.df,
                              by = c(rlang::quo_name(taxon.col)))






  # Calculate the percentage of the specified taxon.
  final.vec <- join.df %>%
    group_by(rlang::UQ(unique.id.col)) %>%
    summarise(TOTAL = sum(rlang::UQ(count.col))) %>%
    original_order(long.df, !!unique.id.col) %>%
    mutate(INDV = taxa_abund(join.df, !!unique.id.col, !!count.col,
                             !!taxon.col, taxon,
                             !!exclusion.col, exclusion.vec),
           #INDV = sum(UQ(count.col)[UQ(taxon.col) %in% taxon]),
           PCT = INDV / TOTAL * 100) %>%
    original_order(long.df, !!unique.id.col) %>%
    dplyr::mutate(PCT = dplyr::if_else(!is.na(PCT), PCT, as.double(0))) %>%
    pull(PCT)
  #----------------------------------------------------------------------------
  return(final.vec)
}
