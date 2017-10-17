#==============================================================================
# Diversity Metrics
#==============================================================================
#'Percentage of a Specified Taxon
#'@description Calculate the percentage of each sample represented by the
#'specified taxon or taxa.
#'@param long.df Taxonomic counts arranged in a long data format.
#'@param unique.id.col The name of the column that contains a unique sampling
#'event ID.
#'@param count.col The name of the column that contains taxanomic counts.
#'@param high.taxa.col The name of the column that contains the taxon or taxa
#'of interest.
#'@param taxon The taxon or taxa of interest. To specify more than one taxa
#'use: c("TAXA1", "TAXA2", "TAXA3").
#'@return A numeric vector of percentages.
#'@export


taxa_div <- function(long.df, unique.id.col, count.col, low.taxa.col = NULL,
                     high.taxa.col, taxon = NULL,
                     job, base.log, q) {
  if (!is.null(low.taxa.col) && is.null(taxon)) {
    paste("Specifying a 'low.taxa.col' is only useful if you",
          "also specify a 'taxon' found in 'low.taxa.col'.") %>%
    stop()
  }
  if (!is.null(taxon) && is.null(low.taxa.col)) {
    paste("Specifying a 'taxon' is only useful if you",
          "also specify the 'low.taxa.col' that contains the 'taxon'.") %>%
      stop()
  }
  #------------------------------------------------------------------------------
  # Prep.
  unique.id.col <- rlang::enquo(unique.id.col)
  high.taxa.col <- rlang::enquo(high.taxa.col)
  count.col <- rlang::enquo(count.col)
  #------------------------------------------------------------------------------
  if (!is.null(taxon) && !is.null(low.taxa.col)) {
    long.df <- long.df %>%
      dplyr::filter(rlang::UQ(low.taxa.col) %in% taxon)
  }
  #------------------------------------------------------------------------------
  agg.df <- long.df %>%
    dplyr::select(!!unique.id.col, !!high.taxa.col, !!count.col) %>%
    dplyr::group_by(!!unique.id.col) %>%
    dplyr::mutate(total = sum(!!count.col))
  #------------------------------------------------------------------------------
  if (!is.null(taxon)) {
    agg.df<- dplyr::filter(agg.df, rlang::UQ(high.taxa.col) %in% rlang::UQ(taxon))
  }
  #------------------------------------------------------------------------------
  if(job %in% c("shannon", "effective_shannon")) {
    final.vec <- agg.df %>%
      dplyr::group_by(!!unique.id.col, !!high.taxa.col, total) %>%
      dplyr::summarize(count = sum(rlang::UQ(count.col))) %>%
      dplyr::mutate(p = count / total) %>%
      dplyr::mutate(log_p = -p * log(p, base.log)) %>%
      dplyr::group_by(!!unique.id.col) %>%
      dplyr::summarize(final = sum(log_p)) %>%
      original_order(long.df, !!unique.id.col) %>%
      dplyr::mutate(final = dplyr::if_else(!is.na(final), final, as.double(0))) %>%
      dplyr::pull(final)
    if(job == "effective_shannon") final.vec <- exp(final.vec)
  }
  #------------------------------------------------------------------------------
  if(job %in% c("simpson", "invsimpson", "gini_simpson", "effective_simpson")) {
    final.vec <- agg.df %>%
      dplyr::group_by(!!unique.id.col, !!high.taxa.col, total) %>%
      dplyr::summarize(count = sum(rlang::UQ(count.col))) %>%
      dplyr::mutate(p = count / total,
                    p = p ^ 2) %>%
      dplyr::group_by(!!unique.id.col) %>%
      dplyr::summarize(final = sum(p)) %>%
      original_order(long.df, !!unique.id.col) %>%
      dplyr::mutate(final = dplyr::if_else(!is.na(final), final, as.double(0))) %>%
      dplyr::pull(final)
    if(job == "simpson") final.vec
    if(job == "gini_simpson") final.vec <- 1 - final.vec
    if(job == "invsimpson") {
      final.vec <- ifelse(final.vec == 0, 0, 1 / final.vec)
    }
    if(job == "effective_simpson") final.vec <- 1 / (1 - final.vec)
  }
  #------------------------------------------------------------------------------
  if(job %in% c("pielou", "margalef", "menhinick")) {
    rich.vec <- taxa_rich(agg.df, !!unique.id.col, !!count.col, !!high.taxa.col)
    if(job == "pielou") final.vec <- log10(rich.vec)
    if(job %in% c("margalef", "menhinick")) {
      abund.vec <- taxa_abund(agg.df, !!unique.id.col, !!count.col, !!high.taxa.col)
      if(job == "margalef") final.vec <- (rich.vec - 1) / log10(abund.vec)
      if(job == "menhinick") final.vec <- rich.vec / sqrt(abund.vec)
    }
  }
  #------------------------------------------------------------------------------
  if(job %in% c("hill", "renyi")) {
    final.vec <- agg.df %>%
      dplyr::group_by(!!unique.id.col, !!high.taxa.col, total) %>%
      dplyr::summarize(count.col = sum(rlang::UQ(count.col))) %>%
      dplyr::mutate(p = count.col / total,
                    p = p ^ q) %>%
      dplyr::group_by(!!unique.id.col) %>%
      dplyr::summarize(final = sum(p)) %>%
      original_order(long.df, !!unique.id.col) %>%
      dplyr::pull(final)
    if(job == "hill") final.vec <- final.vec ^ (1 / (1 - q))
    if(job == "renyi") final.vec <- (1 / (1 - q)) * ln(final.vec)
  }
  #------------------------------------------------------------------------------
  return(final.vec)
}
