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

taxa_pct <- function(long.df, unique.id.col, count.col, taxon.col, taxon,
                     keep.na = FALSE) {
  # Prep.
  unique.id.col <- enquo(unique.id.col)
  taxon.col <- enquo(taxon.col)
  count.col <- enquo(count.col)
  #----------------------------------------------------------------------------
  # Calculate the percentage of the specified taxon.
  final.vec <- long.df %>%
    group_by(rlang::UQ(unique.id.col)) %>%
    summarise(TOTAL = sum(rlang::UQ(count.col)),
              INDV = sum(UQ(count.col)[UQ(taxon.col) %in% taxon]),
              PCT = INDV / TOTAL * 100) %>%
    pull(PCT)
  #----------------------------------------------------------------------------
  return(final.vec)
}

#'Abundance of a Specified Taxon
#'@description Calculate the abundance of each sample represented by the
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

taxa_abund <- function(long.df, unique.id.col, count.col, taxon.col, taxon = NULL,
                       keep.na = FALSE) {
  # Prep.
  unique.id.col <- enquo(unique.id.col)
  taxon.col <- enquo(taxon.col)
  count.col <- enquo(count.col)
  #----------------------------------------------------------------------------
  # Calculate the percentage of the specified taxon.
  if (is.null(taxon)) {
    final.vec <- long.df %>%
      group_by(!!unique.id.col) %>%
      summarise(INDV = sum(UQ(count.col))) %>%
      pull(INDV)
  } else {
    final.vec <- long.df %>%
      group_by(!!unique.id.col) %>%
      summarise(INDV = sum(UQ(count.col)[UQ(taxon.col) %in% taxon])) %>%
      pull(INDV)
  }
  #----------------------------------------------------------------------------
  return(final.vec)
}
#==============================================================================

