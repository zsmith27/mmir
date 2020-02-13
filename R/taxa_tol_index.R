#==============================================================================
# Composition Metrics
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
#'@param taxon The taxon or taxa of interest. To specify more than one taxa
#'use: c("TAXA1", "TAXA2", "TAXA3").
#'@return A numeric vector of percentages.
#'@export


taxa_tol_index <- function(.data, .key_col,
                           .counts_col, .group_col,
                           .filter,
                           .tol_col, na.rm = TRUE) {

  prep.df <- prep_taxa_df(.data = .data,
                          .key_col = {{.key_col}},
                          .unnest_col = {{.unnest_col}},
                          .filter = {{.filter}})

  # .data <- .data %>%
  #   dplyr::mutate({{.group_col}} := trimws({{.group_col}}),
  #                 {{.group_col}} := ifelse({{.group_col}} == "", NA, {{.group_col}}))

  if (na.rm == TRUE) {
    prep.df <- prep.df[!is.na(prep.df[, rlang::quo_name(.group_col)]), ]
  }

  score.vec <- prep.df %>%
    dplyr::group_by({{.key_col}}, {{.group_col}}, {{.tol_col}}) %>%
    dplyr::summarize(count = sum({{.counts_col}})) %>%
    dplyr::filter(!is.na(!{{.tol_col}}),
                  !({{.tol_col}}) %in% c("")) %>%
    dplyr::ungroup() %>%
    dplyr::select({{.key_col}}, {{.tol_col}}, count) %>%
    dplyr::mutate(score = count * ({{.tol_col}})) %>%
    dplyr::group_by({{.key_col}}) %>%
    dplyr::summarize(score = sum(score),
                     taxa = sum(count),
                     score = score / taxa) %>%
    original_order(.data, {{.key_col}}) %>%
    dplyr::pull(score)

  return(score.vec)
}
