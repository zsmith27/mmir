#==============================================================================
#The percent of the sample represented by each taxa per taxon level
#==============================================================================
#'The percentage each taxon makes up of a sample
#'
#'@param long.df Taxonomic counts arranged in a long data format.
#'@param unique.id.col The name of the column that contains a unique sampling
#'event ID.
#'@param count.col The name of the column that contains taxanomic counts.
#'@param taxon.cols The name of the column(s) that contains the taxa
#'of interest.
#'@param job To calculate the percent of each taxon specify "pct". To calculate
#'the richness of each tax
#'@return The percent of the sample represented by each taxa per taxon level.
#'@export
#==============================================================================

taxa_seq_parallel <- function(long.df, unique.id.col, count.col, taxa.cols,
                     high.taxa.col = NULL, keep.na = FALSE, job,
                     base.log = NULL, q = NULL,
                     prefix = FALSE){
  unique.id.col <- rlang::enquo(unique.id.col)
  count.col <- rlang::enquo(count.col)
  #if (!is.null(high.taxa.col)) high.taxa.col <- rlang::enquo(rlang::sym(high.taxa.col))
  kn <- keep.na
  #----------------------------------------------------------------------------

  # Use the detectCores() function to find the number of cores in system
  no_cores <- parallel::detectCores()

  # Setup cluster
  clust <- parallel::makeCluster(no_cores - 1)
  parallel::clusterExport(clust, varlist = c("long.df", "unique.id.col", "count.col",
                                             "job", "prefix",
                                             "taxa.cols", "high.taxa.col",
                                             "base.log", "q"),
                          envir = environment())
  parallel::clusterEvalQ(clust, library("tidyverse"))
  parallel::clusterEvalQ(clust, library("mmir"))

  list.metrics <- parallel::parLapply(clust, taxa.cols, function(col.i) { #lapply(taxa.cols, function(col.i) {
    taxa.vec <- long.df %>%
      dplyr::select(!! rlang::sym(col.i)) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(!! rlang::sym(col.i))) %>%
      pull(!! rlang::sym(col.i))

    taxa.df <- sapply(taxa.vec, function(taxa.i) {
      if (job == "pct") {
        vec.i <- taxa_pct(long.df,
                          unique.id.col = rlang::UQ(unique.id.col),
                          count.col = rlang::UQ(count.col),
                          taxon.col = rlang::UQ(rlang::sym(col.i)),
                          taxon = taxa.i)
      }
      #------------------------------------------------------------------------
      if (job == "rich") {
        if (is.null(high.taxa.col)) stop("high.taxa.col must be speciefied to calculate richness values.")

        vec.i <- taxa_rich(long.df,
                           unique.id.col = rlang::UQ(unique.id.col),
                           low.taxa.col = rlang::UQ(rlang::sym(col.i)),
                           high.taxa.col = rlang::UQ(rlang::sym(high.taxa.col)),
                           taxon = taxa.i)
      }
      #------------------------------------------------------------------------
      if (job == "pct_rich") {
        if (is.null(high.taxa.col)) stop("high.taxa.col must be speciefied to calculate percent richness values.")

        vec.i <- taxa_pct_rich(long.df,
                               unique.id.col = rlang::UQ(unique.id.col),
                               low.taxa.col = rlang::UQ(rlang::sym(col.i)),
                               high.taxa.col = rlang::UQ(rlang::sym(high.taxa.col)),
                               taxon = taxa.i)
      }
      #------------------------------------------------------------------------
      if (job == "abund") {
        vec.i <- taxa_abund(long.df,
                            unique.id.col = rlang::UQ(unique.id.col),
                            count.col = rlang::UQ(count.col),
                            taxon.col = rlang::UQ(rlang::sym(col.i)),
                            taxon = taxa.i,
                            keep.na = kn)
      }
      #------------------------------------------------------------------------
      if (job %in% c("shannon", "effective_shannon", "simpson", "invsimpson",
                     "gini_simpson", "effective_simpson", "pielou",
                     "margalef", "menhinick", "hill", "renyi")) {
        vec.i <- taxa_div(long.df,
                          unique.id.col = rlang::UQ(unique.id.col),
                          count.col = rlang::UQ(count.col),
                          taxon.col = rlang::UQ(rlang::sym(col.i)),
                          taxon = taxa.i,
                          job,
                          base.log,
                          q)
      }
      #------------------------------------------------------------------------
      return(vec.i)
    }) %>%
      as.data.frame() %>%
      rename_all(tolower)

    if (prefix == TRUE) {
      names(taxa.df) <- paste(job, col.i, names(taxa.df), sep = "_")
    } else {
      names(taxa.df) <- paste(job, names(taxa.df), sep = "_")
    }
    return(taxa.df)
  })

  parallel::stopCluster(clust)

  final.df <- bind_cols(list.metrics)

  return(final.df)
}
