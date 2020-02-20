#==============================================================================
# Diversity Metrics
#==============================================================================
#'Percentage of a Specified Taxon
#'@description Calculate the percentage of each sample represented by the
#'specified taxon or taxa.
#'@param .data Taxonomic counts arranged in a long data format.
#'@param .key_col The name of the column that contains a unique sampling
#'event ID.
#'@param .counts_col The name of the column that contains taxanomic counts.
#'@param .group_col The name of the column that contains the taxon or taxa
#'of interest.
#'@param .keep_vec The taxon or taxa of interest. To specify more than one taxa
#'use: c("TAXA1", "TAXA2", "TAXA3").
#'@return A numeric vector of percentages.
#'@importFrom dplyr '%>%'
#'@export


taxa_div <- function(.data, .key_col, .counts_col,
                     .group_col,
                     .filter = NULL,
                     .job, .base_log, .q,
                     .unnest_col = data) {
  #------------------------------------------------------------------------------
  prep.df <- .prep_div(.data = .data,
                      .key_col = {{.key_col}},
                      .counts_col = {{.counts_col}},
                      .group_col = {{.group_col}},
                      .filter = {{.filter}},
                      .unnest_col = {{.unnest_col}})
  #------------------------------------------------------------------------------
  if(.job %in% c("shannon", "effective_shannon")) {
    final.vec <- prep.df %>%
      dplyr::group_by({{.key_col}}, {{.group_col}}, total) %>%
      dplyr::summarize(count = sum({{.counts_col}})) %>%
      dplyr::mutate(p = count / total) %>%
      dplyr::mutate(log_p = -p * log(p, .base_log)) %>%
      dplyr::group_by({{.key_col}}) %>%
      dplyr::summarize(final = sum(log_p, na.rm = TRUE)) %>%
      original_order(.data, {{.key_col}}) %>%
      dplyr::mutate(final = dplyr::if_else(!is.na(final), final, as.double(0))) %>%
      dplyr::pull(final)
    if(.job == "effective_shannon") final.vec <- exp(final.vec)
  }
  #------------------------------------------------------------------------------
  if(.job %in% c("simpson", "invsimpson", "gini_simpson", "effective_simpson")) {
    final.vec <- prep.df %>%
      dplyr::group_by({{.key_col}}, {{.group_col}}, total) %>%
      dplyr::summarize(count = sum({{.counts_col}})) %>%
      dplyr::mutate(p = count / total,
                    p = p ^ 2) %>%
      dplyr::group_by({{.key_col}}) %>%
      dplyr::summarize(final = sum(p, na.rm = TRUE)) %>%
      original_order(.data, {{.key_col}}) %>%
      dplyr::mutate(final = dplyr::if_else(!is.na(final),
                                           as.double(final),
                                           as.double(0))) %>%
      dplyr::pull(final)
    if(.job == "simpson") final.vec
    if(.job == "gini_simpson") {
      final.vec <- ifelse(final.vec > 0, 1 - final.vec, 0)
    }
    if(.job == "invsimpson") {
      final.vec <- ifelse(final.vec != 0, 1 / final.vec, 0)
    }
    if(.job == "effective_simpson"){
      final.vec <- ifelse(final.vec != 1, 1 / (1 - final.vec), 0)
    }
  }
  #------------------------------------------------------------------------------
  if(.job %in% c("pielou", "margalef", "menhinick")) {
    rich.vec <- taxa_rich(.data = .data,
                          .key_col = {{.key_col}},
                          .group_col = {{.group_col}},
                          .filter = {{.filter}},
                          .unnest_col = {{.unnest_col}})
    if(.job == "pielou") final.vec <- log10(rich.vec)
    if(.job %in% c("margalef", "menhinick")) {
      abund.vec <- taxa_abund(.data = .data,
                            .key_col = {{.key_col}},
                            .counts_col = {{.counts_col}},
                            .filter = {{.filter}},
                            .unnest_col = {{.unnest_col}})
      if(.job == "margalef") {
        final.vec <- ifelse(rich.vec != 1 & abund.vec != 0,
                            (rich.vec - 1) / log10(abund.vec),
                            0)

      }
      if(.job == "menhinick") {
        final.vec <- ifelse(abund.vec != 0,
                            rich.vec / sqrt(abund.vec),
                            0)
      }
    }
  }
  #------------------------------------------------------------------------------
  if(.job %in% c("hill", "renyi")) {
    final.vec <- prep.df %>%
      dplyr::group_by({{.key_col}}, {{.group_col}}, total) %>%
      dplyr::summarize(.counts_col = sum({{.counts_col}}),
                       na.rm = TRUE) %>%
      dplyr::mutate(p = .counts_col / total,
                    p = p ^ .q) %>%
      dplyr::group_by({{.key_col}}) %>%
      dplyr::summarize(final = sum(p, na.rm = TRUE)) %>%
      original_order(.data, {{.key_col}}) %>%
      dplyr::pull(final)
    if(.job == "hill") final.vec <- final.vec ^ (1 / (1 - .q))
    if(.job == "renyi") final.vec <- (1 / (1 - .q)) * log(final.vec)
  }
  #------------------------------------------------------------------------------
  return(final.vec)
}


.prep_div <- function(.data, .key_col, .counts_col, .group_col,
                     .filter = NULL,
                     .unnest_col = data) {

  prep.df <- prep_taxa_df(.data = .data,
                          .key_col = {{.key_col}},
                          .unnest_col = {{.unnest_col}},
                          .filter = {{.filter}})

  final.df <- tidyr::complete(prep.df, {{.key_col}}, {{.group_col}}) %>%
    dplyr::mutate({{.counts_col}} := dplyr::if_else(is.na({{.counts_col}}),
                                                    as.double(0),
                                                    as.double({{.counts_col}}))) %>%
    dplyr::group_by({{.key_col}}) %>%
    dplyr::mutate(total = sum({{.counts_col}})) %>%
    dplyr::ungroup()

  return(final.df)
}

