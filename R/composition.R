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
                     exclusion.col = NULL, exclusion.vec = NULL) {
  # Prep.
  unique.id.col <- enquo(unique.id.col)
  taxon.col <- enquo(taxon.col)
  count.col <- enquo(count.col)
  exclusion.col <- enquo(exclusion.col)
  #----------------------------------------------------------------------------
  #long.df <- long.df %>%
  #  dplyr::filter((!!count.col) > 0)
  #----------------------------------------------------------------------------
  if (rlang::quo_is_null(exclusion.col)) {
    # Aggregate taxonomic counts at the specified taxonomic levels.
    taxa.counts <- long.df %>%
      dplyr::select(!!unique.id.col, !!taxon.col, !!count.col)
  } else {
    taxa.counts <- long.df %>%
      dplyr::select(!!unique.id.col, !!taxon.col,
                    !!count.col, !!exclusion.col) %>%
      dplyr::filter(!rlang::UQ(exclusion.col) %in% exclusion.vec)
  }
  #----------------------------------------------------------------------------
  # Calculate the percentage of the specified taxon.
  final.vec <- taxa.counts%>%
    group_by(rlang::UQ(unique.id.col)) %>%
    summarise(TOTAL = sum(rlang::UQ(count.col))) %>%
    original_order(long.df, !!unique.id.col) %>%
    mutate(INDV = taxa_abund(taxa.counts, !!unique.id.col, !!count.col,
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
                       exclusion.col = NULL, exclusion.vec = NULL) {
  # Prep.
  unique.id.col <- enquo(unique.id.col)
  taxon.col <- enquo(taxon.col)
  count.col <- enquo(count.col)
  exclusion.col <- enquo(exclusion.col)
  #----------------------------------------------------------------------------
  #long.df <- long.df %>%
  #  dplyr::filter((!!count.col) > 0)
  #----------------------------------------------------------------------------
  if (rlang::quo_is_null(exclusion.col)) {
    # Aggregate taxonomic counts at the specified taxonomic levels.
    taxa.counts <- long.df %>%
      dplyr::select(!!unique.id.col, !!taxon.col, !!count.col)
  } else {
    taxa.counts <- long.df %>%
      dplyr::select(!!unique.id.col, !!taxon.col,
                    !!count.col, !!exclusion.col) %>%
      dplyr::filter(!rlang::UQ(exclusion.col) %in% exclusion.vec)
  }
  #----------------------------------------------------------------------------
  # Calculate the percentage of the specified taxon.
  if (is.null(taxon)) {
    final.vec <- taxa.counts %>%
      group_by(!!unique.id.col) %>%
      summarise(INDV = sum(UQ(count.col))) %>%
      original_order(long.df, !!unique.id.col) %>%
      pull(INDV)
  } else {
    final.vec <- taxa.counts %>%
      group_by(!!unique.id.col) %>%
      summarise(INDV = sum(UQ(count.col)[UQ(taxon.col) %in% taxon])) %>%
      original_order(long.df, !!unique.id.col) %>%
      dplyr::mutate(INDV = as.numeric(INDV),
                    INDV = dplyr::if_else(!is.na(INDV), INDV, as.double(0))) %>%
      pull(INDV)
  }
  #----------------------------------------------------------------------------
  return(final.vec)
}
#==============================================================================

