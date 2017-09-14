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
#'@param taxon.col The name of the column that contains the taxon or taxa
#'of interest.
#'@param taxon The taxon or taxa of interest. To specify more than one taxa
#'use: c("TAXA1", "TAXA2", "TAXA3").
#'@return A numeric vector of percentages.
#'@export


taxa_div <- function(long.df, unique.id.col, count.col, taxon.col, taxon = NULL,
                     job, base.log, q) {
  # Prep.
  unique.id.col <- enquo(unique.id.col)
  taxon.col <- enquo(taxon.col)
  count.col <- enquo(count.col)
  #------------------------------------------------------------------------------
  long.df <- long.df %>%
    dplyr::select(!!unique.id.col, !!taxon.col, !!count.col) %>%
    dplyr::group_by(!!unique.id.col) %>%
    dplyr::mutate(total = sum(!!count.col)) %>%
    dplyr::rename(UNIQUE_ID = !!unique.id.col)

  distinct.df <- long.df %>%
    dplyr::select(UNIQUE_ID) %>%
    dplyr::distinct()
  #------------------------------------------------------------------------------
  if (!is.null(taxon)) {
    long.df <- dplyr::filter(long.df, rlang::UQ(taxon.col) %in% rlang::UQ(taxon))
  }
  #------------------------------------------------------------------------------
  if(job %in% c("shannon", "effective_shannon")) {
    final.vec <- long.df %>%
      dplyr::group_by(UNIQUE_ID, !!taxon.col, total) %>%
      dplyr::summarize(count = sum(rlang::UQ(count.col))) %>%
      dplyr::mutate(p = count / total) %>%
      dplyr::mutate(log_p = -p * log(p, base.log)) %>%
      dplyr::group_by(UNIQUE_ID) %>%
      dplyr::summarize(final = sum(log_p)) %>%
      dplyr::right_join(distinct.df, by = "UNIQUE_ID") %>%
      dplyr::mutate(final = dplyr::if_else(!is.na(final), final, as.double(0))) %>%
      dplyr::pull(final)
    if(job == "effective_shannon") final.vec <- exp(final.vec)
  }
  #------------------------------------------------------------------------------
  if(job %in% c("simpson", "invsimpson", "gini_simpson", "effective_simpson")) {
    final.vec <- long.df %>%
      dplyr::group_by(UNIQUE_ID, !!taxon.col, total) %>%
      dplyr::summarize(count = sum(rlang::UQ(count.col))) %>%
      dplyr::mutate(p = count / total,
                    p = p ^ 2) %>%
      dplyr::group_by(UNIQUE_ID) %>%
      dplyr::summarize(final = sum(p)) %>%
      dplyr::right_join(distinct.df, by = "UNIQUE_ID") %>%
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
    rich.vec <- taxon_richness(long.df, UNIQUE_ID, !!count.col, !!taxon.col)
    if(job == "pielou") final.vec <- log10(rich.vec)
    if(job %in% c("margalef", "menhinick")) {
      abund.vec <- abund_taxon(long.df, UNIQUE_ID, !!count.col, !!taxon.col)
      if(job == "margalef") final.vec <- (rich.vec - 1) / log10(abund.vec)
      if(job == "menhinick") final.vec <- rich.vec / sqrt(abund.vec)
    }
  }
  #------------------------------------------------------------------------------
  if(job %in% c("hill", "renyi")) {
    final.vec <- long.df %>%
      dplyr::group_by(UNIQUE_ID, !!taxon.col, total) %>%
      dplyr::summarize(count.col = sum(rlang::UQ(count.col))) %>%
      dplyr::mutate(p = count.col / total,
                    p = p ^ q) %>%
      dplyr::group_by(UNIQUE_ID) %>%
      dplyr::summarize(final = sum(p)) %>%
      dplyr::pull(final)
    if(job == "hill") final.vec <- final.vec ^ (1 / (1 - q))
    if(job == "renyi") final.vec <- (1 / (1 - q)) * ln(final.vec)
  }
  #------------------------------------------------------------------------------
  return(final.vec)
}
