#==============================================================================
# Diversity Metrics
#==============================================================================
#'Percentage of a Specified Taxon
#'@description Calculate the percentage of each sample represented by the
#'specified taxon or taxa.
#'@param long.df Taxonomic counts arranged in a long data format.
#'@param count.col The name of the column that contains taxanomic counts.
#'@param high.taxa.col The name of the column that contains the taxon or taxa
#'of interest.
#'@param taxon The taxon or taxa of interest. To specify more than one taxa
#'use: c("TAXA1", "TAXA2", "TAXA3").
#'@return A numeric vector of percentages.
#'@export

taxa_div <- function(long.df, count.col, low.taxa.col = NULL,
                     high.taxa.col, taxon = NULL,
                     job, base.log, q) {
  #------------------------------------------------------------------------------
  # Prep.
  high.taxa.col <- rlang::enquo(high.taxa.col)
  if (!is.null(low.taxa.col)) low.taxa.col <- rlang::enquo(low.taxa.col)
  count.col <- rlang::enquo(count.col)
  count.col.name <- rlang::sym(rlang::quo_name(count.col))
  #------------------------------------------------------------------------------
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
  if (!is.null(taxon) && !rlang::quo_is_null(low.taxa.col)) {
    print("Test")
    prep_div <- function(long.df, count.col, low.taxa.col,
                         high.taxa.col, taxon = NULL) {
      high.taxa.col <- rlang::enquo(high.taxa.col)
      low.taxa.col <- rlang::enquo(low.taxa.col)
      count.col <- rlang::enquo(count.col)
      count.col.name <- rlang::sym(rlang::quo_name(count.col))

      if(!is.vector(taxon)) stop("The 'taxon' object must be a vector.")

      long.sub <- tidyr::complete(long.df, !!!syms(quo_name(low.taxa.col)))
      long.sub <- long.sub %>%
        dplyr::mutate(!!count.col.name := dplyr::if_else(is.na(!!count.col), 0, as.double(!!count.col))) %>%
        dplyr::filter((!!low.taxa.col) %in% taxon)# %>%
      # dplyr::rename(quo_name(count.col) = count)

      agg.df <- long.sub %>%
        # dplyr::select(!!unique.id.col,
        #               (!!low.taxa.col),
        #               !!high.taxa.col, !!count.col) %>%
        dplyr::group_by(!!unique.id.col) %>%
        dplyr::mutate(total = sum(!!count.col)) %>%
        dplyr::ungroup()

      return(agg.df)
    }
    agg.df <- prep_div(long.df,
                       !!count.col,
                       !!low.taxa.col,
                       !!high.taxa.col,
                       taxon)
  } else {
    agg.df <- long.df %>%
      dplyr::select(!!high.taxa.col, !!count.col) %>%
      dplyr::mutate(total = sum(!!count.col)) %>%
      dplyr::ungroup()
  }
  #------------------------------------------------------------------------------
  if(job %in% c("shannon", "effective_shannon")) {
    final.vec <- agg.df %>%
      dplyr::group_by(!!high.taxa.col, total) %>%
      dplyr::summarize(count = sum(!!count.col)) %>%
      dplyr::mutate(p = count / total) %>%
      dplyr::mutate(log_p = -p * log(p, base.log)) %>%
      dplyr::ungroup() %>%
      dplyr::summarize(final = sum(log_p)) %>%
      dplyr::mutate(final = dplyr::if_else(!is.na(final), final, as.double(0))) %>%
      dplyr::pull(final)
    if(job == "effective_shannon") final.vec <- exp(final.vec)
  }
  #------------------------------------------------------------------------------
  if(job %in% c("simpson", "invsimpson", "gini_simpson", "effective_simpson")) {
    final.vec <- agg.df %>%
      dplyr::group_by(!!high.taxa.col, total) %>%
      dplyr::summarize(count = sum(!!count.col)) %>%
      dplyr::mutate(p = count / total,
                    p = p ^ 2) %>%
      dplyr::ungroup() %>%
      dplyr::summarize(final = sum(p)) %>%
      dplyr::mutate(final = dplyr::if_else(!is.na(final), final, as.double(0))) %>%
      dplyr::pull(final)
    if (job == "simpson") final.vec
    if (job == "gini_simpson") final.vec <- ifelse(final.vec > 0, 1 - final.vec, 0)
    if (job == "invsimpson") {
      final.vec <- ifelse(final.vec == 0, 0, 1 / final.vec)
    }
    if (job == "effective_simpson") final.vec <- 1 / (1 - final.vec)
  }
  #------------------------------------------------------------------------------
  if (job %in% c("pielou", "margalef", "menhinick")) {
    rich.vec <- taxa_rich(long.df = agg.df,
                          low.taxa.col = !!high.taxa.col)
    if (job == "pielou") final.vec <- log10(rich.vec)
    if (job %in% c("margalef", "menhinick")) {
      abund.vec <- taxa_abund(long.df = agg.df,
                              count.col = !!count.col,
                              taxon.col = !!high.taxa.col)
      if(job == "margalef") final.vec <- (rich.vec - 1) / log10(abund.vec)
      if(job == "menhinick") final.vec <- rich.vec / sqrt(abund.vec)
    }
  }
  #------------------------------------------------------------------------------
  if(job %in% c("hill", "renyi")) {
    final.vec <- agg.df %>%
      dplyr::group_by(!!high.taxa.col, total) %>%
      dplyr::summarize(count.col = sum(!!count.col)) %>%
      ungroup() %>%
      dplyr::mutate(p = count.col / total,
                    p = p ^ q) %>%
      dplyr::ungroup() %>%
      dplyr::summarize(final = sum(p)) %>%
      dplyr::pull(final)
    if(job == "hill") final.vec <- final.vec ^ (1 / (1 - q))
    if(job == "renyi") final.vec <- (1 / (1 - q)) * log(final.vec)
  }
  #------------------------------------------------------------------------------
  return(final.vec)
}


# test <- taxa_div(onondaga,
#          unique.id.col = unique_id,
#          count.col = reporting_value,
#          low.taxa.col = order,
#          high.taxa.col = genus,
#          taxon = c("ephemeroptera", "plecoptera", "trichoptera"),
#          job = "shannon", base.log = 2)
