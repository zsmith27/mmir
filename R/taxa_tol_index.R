# ==============================================================================
# Composition Metrics
# ==============================================================================
#' Tolerance Index
#' @description Calculate the percentage of each sample represented by the
#' specified taxon or taxa.
#' @param .dataframe A data frame where each row should represent the number of
#' individuals enumerated for a single taxon collected during a single sampling event.
#' @param .key_col One unquoted column name that represents a key (i.e., unique ID)
#'  for a sampling event for which to group (i.e., aggregate) the data.
#' @param .counts_col One unquoted column name that represents taxonomic counts.
#' @param .filter A logical statement to subset the data frame prior to calculating
#' the metric of interest.
#' @param .unnest_col One unqouted column name that represents nested data.
#'  If this column is NULL (default), then the data will not be unnested.
#' @param .tol_col One unqouted column name that represents numeric tolerance values.
#' @param na.rm A logical value indicating if taxa with missing tolerance values (.tol_val = NA)
#' should be removed prior to calculating the tolerance index. The default is TRUE, NA values should
#' will be removed, becuase missing values will be effectively treated as zeros during calculations;
#' this will incorrectly skew the final value to the lower range of the index spectrum.
#' @return A numeric vector.
#' @importFrom rlang .data
#' @export


taxa_tol_index <- function(.dataframe, .key_col,
                           .counts_col,
                           .filter = NULL,
                           .unnest_col = NULL,
                           .tol_col,
                           na.rm = TRUE) {
  prep.df <- prep_taxa_df(
    .dataframe = .dataframe,
    .key_col = {{ .key_col }},
    .unnest_col = {{ .unnest_col }},
    .filter = {{ .filter }}
  )

  if (na.rm == TRUE) {
    prep.df <- dplyr::filter(prep.df, !is.na({{ .tol_col }}))
  }

  score.vec <- prep.df %>%
    dplyr::mutate(score = {{ .counts_col }} * {{ .tol_col }}) %>%
    dplyr::group_by({{ .key_col }}) %>%
    dplyr::summarize(
      score = sum(.data$score) / sum({{ .counts_col }})
    ) %>%
    original_order(.dataframe, {{ .key_col }}) %>%
    dplyr::pull(.data$score)

  return(score.vec)
}
