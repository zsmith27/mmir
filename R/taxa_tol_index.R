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

taxa_tol_index <- function(long.df, unique.id.col, count.col, taxon.col, tol.col, na.rm = TRUE) {
  unique.id.col <- rlang::enquo(unique.id.col)
  tol.col <- rlang::enquo(tol.col)
  count.col <- rlang::enquo(count.col)
  taxon.col <- rlang::enquo(taxon.col)

  long.df <- long.df %>%
    dplyr::mutate(!!taxon.col := trimws(!!taxon.col),
                  !!taxon.col := ifelse(!!taxon.col == "", NA, !!taxon.col))

  if (na.rm == TRUE) {
    long.df <- long.df[!is.na(long.df[, rlang::quo_name(taxon.col)]), ]
  }

  score.vec <- long.df %>%
    dplyr::group_by(!!unique.id.col, !!taxon.col, !!tol.col) %>%
    dplyr::summarize(count = sum(!!count.col)) %>%
    dplyr::filter(!is.na(!!!tol.col),
                  !(!!tol.col) %in% c("")) %>%
    dplyr::ungroup() %>%
    dplyr::select(!!unique.id.col, !!tol.col, count) %>%
    dplyr::mutate(score = count * (!!tol.col)) %>%
    dplyr::group_by(!!unique.id.col) %>%
    dplyr::summarize(score = sum(score),
                     taxa = sum(count),
                     score = score / taxa) %>%
    original_order(long.df, !!unique.id.col) %>%
    dplyr::pull(score)

  return(score.vec)
}
