# ==============================================================================
# Diversity Metric
# ==============================================================================
#' Taxonomic Diversity
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
#' @param .group_col One unquoted column name that represents a taxomic rank
#'  or group of interest.
#' @param .job A character string specifying the diversity metric of interest.
#' Below is a list of exceptable inputs:
##' \itemize{
##'  \item{"shannon"}{Description needed}
##'  \item{"effective_shannon"}{Description needed}
##'  \item{"simpson""}{Description needed}
##'  \item{"invsimpson"}{Description needed}
##'  \item{"gini_simpson"}{Description needed}
##'  \item{"effective_simpson"}{Description needed}
##'  \item{"pielou"}{Description needed}
##'  \item{"margalef"}{Description needed}
##'  \item{"menhinick"}{Description needed}
##'  \item{"hill"}{Description needed}
##'  \item{"renyi"}{Description needed}
##' }
#' @param .base_log The base log value used during the calculation of
#' Shannon Diversity index ("shannon") or Effective Shannon Diversity ("effective_shannon").
#' The default value is two.
#' @param .q The exponent used during the calculation of Hill Numbers ("hill") and
#' Renyi Entropy ("renyi").
#' @return A numeric vector.
#' @importFrom rlang .data
#' @export

taxa_div <- function(.dataframe, .key_col, .counts_col,
                     .group_col,
                     .filter = NULL,
                     .job, .base_log = 2, .q,
                     .unnest_col = NULL) {
  #------------------------------------------------------------------------------
  prep.df <- .prep_div(
    .dataframe = .dataframe,
    .key_col = {{ .key_col }},
    .counts_col = {{ .counts_col }},
    .group_col = {{ .group_col }},
    .filter = {{ .filter }},
    .unnest_col = {{ .unnest_col }}
  )
  #------------------------------------------------------------------------------
  if (.job %in% c("shannon", "effective_shannon")) {
    final.vec <- prep.df %>%
      dplyr::group_by({{ .key_col }}, {{ .group_col }}, .data$total) %>%
      dplyr::summarize(count = sum({{ .counts_col }})) %>%
      dplyr::mutate(p = .data$count / .data$total) %>%
      dplyr::mutate(log_p = -.data$p * log(.data$p, .base_log)) %>%
      dplyr::group_by({{ .key_col }}) %>%
      dplyr::summarize(final = sum(.data$log_p, na.rm = TRUE)) %>%
      original_order(.dataframe, {{ .key_col }}) %>%
      dplyr::mutate(final = dplyr::if_else(!is.na(.data$final),
                                           .data$final,
                                           as.double(0))) %>%
      dplyr::pull(.data$final)
    if (.job == "effective_shannon") final.vec <- exp(final.vec)
  }
  #------------------------------------------------------------------------------
  if (.job %in% c("simpson", "invsimpson", "gini_simpson", "effective_simpson")) {
    final.vec <- prep.df %>%
      dplyr::group_by({{ .key_col }}, {{ .group_col }}, .data$total) %>%
      dplyr::summarize(count = sum({{ .counts_col }})) %>%
      dplyr::mutate(
        p = .data$count / .data$total,
        p = .data$p^2
      ) %>%
      dplyr::group_by({{ .key_col }}) %>%
      dplyr::summarize(final = sum(.data$p, na.rm = TRUE)) %>%
      original_order(.dataframe, {{ .key_col }}) %>%
      dplyr::mutate(final = dplyr::if_else(
        !is.na(.data$final),
        as.double(.data$final),
        as.double(0)
      )) %>%
      dplyr::pull(.data$final)
    if (.job == "simpson") final.vec
    if (.job == "gini_simpson") {
      final.vec <- ifelse(final.vec > 0, 1 - final.vec, 0)
    }
    if (.job == "invsimpson") {
      final.vec <- ifelse(final.vec != 0, 1 / final.vec, 0)
    }
    if (.job == "effective_simpson") {
      final.vec <- ifelse(final.vec != 1, 1 / (1 - final.vec), 0)
    }
  }
  #------------------------------------------------------------------------------
  if (.job %in% c("pielou", "margalef", "menhinick")) {
    rich.vec <- taxa_rich(
      .dataframe = .dataframe,
      .key_col = {{ .key_col }},
      .group_col = {{ .group_col }},
      .filter = {{ .filter }},
      .unnest_col = {{ .unnest_col }}
    )
    if (.job == "pielou") final.vec <- log10(rich.vec)
    if (.job %in% c("margalef", "menhinick")) {
      abund.vec <- taxa_abund(
        .dataframe = .dataframe,
        .key_col = {{ .key_col }},
        .counts_col = {{ .counts_col }},
        .filter = {{ .filter }},
        .unnest_col = {{ .unnest_col }}
      )
      if (.job == "margalef") {
        final.vec <- ifelse(rich.vec != 1 & abund.vec != 0,
          (rich.vec - 1) / log10(abund.vec),
          0
        )
      }
      if (.job == "menhinick") {
        final.vec <- ifelse(abund.vec != 0,
          rich.vec / sqrt(abund.vec),
          0
        )
      }
    }
  }
  #------------------------------------------------------------------------------
  if (.job %in% c("hill", "renyi")) {
    final.vec <- prep.df %>%
      dplyr::group_by({{ .key_col }}, {{ .group_col }}, .data$total) %>%
      dplyr::summarize(
        .counts_col = sum({{ .counts_col }}),
        na.rm = TRUE
      ) %>%
      dplyr::mutate(
        p = {{ .counts_col }} / .data$total,
        p = .data$p^.q
      ) %>%
      dplyr::group_by({{ .key_col }}) %>%
      dplyr::summarize(final = sum(.data$p, na.rm = TRUE)) %>%
      original_order(.dataframe, {{ .key_col }}) %>%
      dplyr::pull(.data$final)
    if (.job == "hill") final.vec <- final.vec^(1 / (1 - .q))
    if (.job == "renyi") final.vec <- (1 / (1 - .q)) * log(final.vec)
  }
  #------------------------------------------------------------------------------
  return(final.vec)
}

.prep_div <- function(.dataframe, .key_col, .counts_col, .group_col,
                      .filter = NULL,
                      .unnest_col = NULL) {
  prep.df <- prep_taxa_df(
    .dataframe = .dataframe,
    .key_col = {{ .key_col }},
    .unnest_col = {{ .unnest_col }},
    .filter = {{ .filter }}
  )

  final.df <- tidyr::complete(prep.df, {{ .key_col }}, {{ .group_col }}) %>%
    dplyr::mutate({{ .counts_col }} := dplyr::if_else(
      is.na({{ .counts_col }}),
      as.double(0),
      as.double({{ .counts_col }})
    )) %>%
    dplyr::group_by({{ .key_col }}) %>%
    dplyr::mutate(total = sum({{ .counts_col }})) %>%
    dplyr::ungroup()

  return(final.df)
}
