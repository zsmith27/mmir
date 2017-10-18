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

taxa_tol_index <- function(long.df, unique.id.col, count.col, tol.col) {
  unique.id.col <- rlang::enquo(unique.id.col)
  tol.col <- rlang::enquo(tol.col )
  count.col <- rlang::enquo(count.col )

  score.vec <- long.df %>%
    filter(!is.na(!!!tol.col),
           !rlang::UQ(tol.col) %in% c("")) %>%
    select(!!unique.id.col, !!tol.col, !!count.col) %>%
    mutate(score = rlang::UQ(count.col) * rlang::UQ(tol.col)) %>%
    group_by(!!unique.id.col) %>%
    summarize(score = sum(score),
              taxa = sum(!!count.col),
              score = score / taxa) %>%
    original_order(long.df, !!unique.id.col) %>%
    pull(score)

  return(score.vec)
}
