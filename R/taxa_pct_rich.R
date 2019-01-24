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


taxa_pct_rich <- function(long.df, low.taxa.col,
                          high.taxa.col, taxon = NULL,
                          exclusion.col = NULL, exclusion.vec = NULL) {

  low.taxa.col <- rlang::enquo(low.taxa.col)
  high.taxa.col <- rlang::enquo(high.taxa.col)
  exclusion.col <- rlang::enquo(exclusion.col)

  if (is.null(taxon)) stop("Must specify 'taxon'.")
  if (!rlang::quo_is_null(exclusion.col) && is.null(exclusion.vec)) {
    stop("Specifying an exclusion.col also requires that you specify the
         objects you want to exclude (i.e. exclusion.vec) from that column.")
  }
  if (!is.null(exclusion.vec) && rlang::quo_is_null(exclusion.col)) {
    stop("Specifying an exclusion.vec also requires that you specify the
         column (i.e. exclusion.col) from which to exclude the objects.")
  }
  #----------------------------------------------------------------------------

  if (nrow(long.df) < 1) return(0)
  if (!is.null(taxon) &&
      nrow(dplyr::filter(long.df, (!!low.taxa.col) %in% taxon)) < 1) return(0)
  #----------------------------------------------------------------------------
  if (!rlang::quo_is_null(exclusion.col)) {
    long.df <- dplyr::filter(long.df, !(!!exclusion.col) %in% exclusion.vec)
  }
  # taxa.counts <- taxa.counts[complete.cases(taxa.counts),]
  #----------------------------------------------------------------------------
  final.vec <- long.df %>%
    dplyr::summarize(rich = taxa_rich(long.df = .,
                                      !!high.taxa.col),
                  taxa_rich = taxa_rich(long.df = .,
                                        !!low.taxa.col,
                                        !!high.taxa.col,
                                        taxon,
                                        exclusion.col = !!exclusion.col,
                                        exclusion.vec = exclusion.vec),
                  pct_rich = dplyr::if_else(taxa_rich == 0, 0, taxa_rich / rich * 100)) %>%
    dplyr::pull(pct_rich)
  #----------------------------------------------------------------------------
  return(final.vec)
  }
