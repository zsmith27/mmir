#==============================================================================
#The percent of the sample represented by each taxa per taxon level
#==============================================================================
#'The percentage each taxon makes up of a sample
#'
#'@param .data Taxonomic counts arranged in a long data format.
#'@param .key_col The name of the column that contains a unique sampling
#'event ID.
#'@param .counts_col The name of the column that contains taxanomic counts.
#'@param .filter_cols_vecs The name of the column(s) that contains the taxa
#'of interest.
#'@param .job To calculate the percent of each taxon specify "pct". To calculate
#'the richness of each tax
#'@return The percent of the sample represented by each taxa per taxon level.
#'@export
#==============================================================================

taxa_seq <- function(.data, .key_col, .counts_col, .filter_cols_vec,
                     .group_col,
                      .keep_na = FALSE, .job,
                     .base_log= NULL, .q = NULL){
  prep.df <- prep_taxa_df(.data = .data,
                          .key_col = {{.key_col}},
                          .unnest_col = {{.unnest_col}},
                          .filter = NULL)
  kn <- .keep_na
  #----------------------------------------------------------------------------
  list.metrics <- lapply(.filter_cols_vec, function(col.i) {
    # print(col.i)
    col.i <- rlang::sym(col.i)
    taxa.vec <- prep.df %>%
      dplyr::select({{col.i}}) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(.)) %>%
      pull({{col.i}}) %>%
      stringr::str_trim()

    taxa.vec <- taxa.vec[stringr::str_length(taxa.vec) > 0]

    if (length(taxa.vec) == 0) return(data.frame())

    # length(taxa.vec)
    # str_length(taxa.vec)
    # grepl("", taxa.vec)
    taxa.df <- sapply(taxa.vec, function(taxa.i) {
      #------------------------------------------------------------------------
      if (.job == "abund") {
        vec.i <- taxa_abund(.data,
                            .key_col = {{.key_col}},
                            .counts_col = {{.counts_col}},
                            .filter = {{col.i}} %in% taxa.i)
      }
      #------------------------------------------------------------------------
      if (.job == "pct") {
        vec.i <- taxa_pct(.data,
                          .key_col = {{.key_col}},
                          .counts_col = {{.counts_col}},
                          .filter = {{col.i}} %in% taxa.i)
      }
      #------------------------------------------------------------------------
      if (.job == "rich") {

        vec.i <- taxa_rich(.data,
                           .key_col = {{.key_col}},
                           .filter = {{col.i}} %in% taxa.i,
                           .group_col = {{.group_col}})
      }
      #------------------------------------------------------------------------
      if (.job == "pct_rich") {

        vec.i <- taxa_pct_rich(.data,
                               .key_col = {{.key_col}},
                               .filter = {{col.i}} %in% taxa.i,
                               .group_col = {{.group_col}})
      }
      #------------------------------------------------------------------------
      if (.job %in% c("shannon", "effective_shannon", "simpson", "invsimpson",
                     "gini_simpson", "effective_simpson", "pielou",
                     "margalef", "menhinick", "hill", "renyi")) {
        vec.i <- taxa_div(.data,
                          .key_col = {{.key_col}},
                          .counts_col = {{.counts_col}},
                          .group_col = {{.group_col}},
                          .filter = {{col.i}} %in% taxa.i,
                          .job = .job,
                          .base_log = .base_log,
                          .q = .q)
      }
      #------------------------------------------------------------------------
      return(vec.i)
    }) %>%
      as.data.frame() %>%
      dplyr::rename_all(tolower)

    names(taxa.df) <- paste(.job, rlang::quo_name(rlang::enquo(.group_col)), names(taxa.df), sep = "_")
    return(taxa.df)
  })
  final.df <- bind_cols(list.metrics)
  return(final.df)
}
