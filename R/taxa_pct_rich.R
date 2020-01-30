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
#'@param .filter_vec The taxon or taxa of interest. To specify more than one taxa
#'use: c("TAXA1", "TAXA2", "TAXA3").
#'@return A numeric vector of percentages.
#'@export


<<<<<<< HEAD
taxa_pct_rich <- function(.data, .key_col, .group_col,
                          .filter_col, .filter_vec = NULL,
                          exclusion.col = NULL, exclusion.vec = NULL,
                          unnest.cols = data) {

  .data <- tidyr::unnest(.data, cols = !!rlang::enquo(unnest.cols))

  .key_col <- rlang::enquo(.key_col)
  .group_col <- rlang::enquo(.group_col)
  .filter_col <- rlang::enquo(.filter_col)
=======
taxa_pct_rich <- function(long.df, unique.id.col, low.taxa.col,
                          high.taxa.col, taxon = NULL,
                          exclusion.col = NULL, exclusion.vec = NULL,
                          unnest.cols = data) {

  long.df <- tidyr::unnest(long.df, cols = !!rlang::enquo(unnest.cols))

  unique.id.col <- rlang::enquo(unique.id.col)
  low.taxa.col <- rlang::enquo(low.taxa.col)
  high.taxa.col <- rlang::enquo(high.taxa.col)
>>>>>>> 639a17f21c077953610adaba81369fd2ce5557f1
  exclusion.col <- rlang::enquo(exclusion.col)

  if (is.null(.filter_vec)) stop("Must specify '.filter_vec'.")
  if (!rlang::quo_is_null(exclusion.col) && is.null(exclusion.vec)) {
    stop("Specifying an exclusion.col also requires that you specify the
         objects you want to exclude (i.e. exclusion.vec) from that column.")
  }
  if (!is.null(exclusion.vec) && rlang::quo_is_null(exclusion.col)) {
    stop("Specifying an exclusion.vec also requires that you specify the
         column (i.e. exclusion.col) from which to exclude the objects.")
  }
  #----------------------------------------------------------------------------
  if (!rlang::quo_is_null(exclusion.col)) {
    .data <- dplyr::filter(.data, !(!!exclusion.col) %in% exclusion.vec)
  }
  #----------------------------------------------------------------------------
  # taxa.counts <- taxa.counts[complete.cases(taxa.counts),]
  #----------------------------------------------------------------------------
  distinct.df <- .data %>%
    dplyr::select(!!.key_col) %>%
    dplyr::distinct()

  final.vec <- distinct.df %>%
    dplyr::mutate(
      rich = taxa_rich(
        .data = .data,
        .key_col = !!.key_col,
        .group_col = !!.filter_col
      ),
      taxa_rich = taxa_rich(
        .data = .data,
        .key_col = !!.key_col,
        .group_col = !!.group_col,
        .filter_col = !!.filter_col,
        .filter_vec = .filter_vec,
        exclusion.col = !!exclusion.col,
        exclusion.vec = exclusion.vec
      ),
      pct_rich = if_else(taxa_rich == 0, 0, taxa_rich / rich * 100)
    ) %>%
    original_order(.data,!!.key_col) %>%
    pull(pct_rich)
  #----------------------------------------------------------------------------
  return(final.vec)
  }
