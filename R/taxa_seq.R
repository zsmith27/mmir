#' Sequence Through Taxonomic Metrics
#' @description This function is a wrapper for the other "taxa_" functions that sequences
#' through unique values within specified columns. The intent is to provide the ability to
#' quickly calculate metrics for a large number of taxa without having to call a "taxa_"
#' function for each taxon of interest.
#' @param .dataframe A data frame where each row should represent the number of
#' individuals enumerated for a single taxon collected during a single sampling event.
#' @param .key_col One unquoted column name that represents a key (i.e., unique ID)
#'  for a sampling event for which to group (i.e., aggregate) the data.
#' @param .counts_col One unquoted column name that represents taxonomic counts.
#' @param .unnest_col One unqouted column name that represents nested data.
#'  If this column is NULL (default), then the data will not be unnested.
#' @param .filter_cols_vec A quoted vector of column names for which
#' the function will sequence through each unique value to perform the specified .job.
#' @param .group_col One unquoted column name that represents a taxomic rank
#'  or group of interest.
#' @param .job A character string specifying the metric of interest.
#' Below is a list of exceptable inputs:
##' \itemize{
##'  \item{"abund"}{Description needed}
##'  \item{"pct"}{Description needed}
##'  \item{"rich"}{Description needed}
##'  \item{"pct_rich"}{Description needed}
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
#' @return A data frame where each column represents numeric metric values.
#' @importFrom rlang .data
#' @export
# ==============================================================================

taxa_seq <- function(.dataframe, .key_col, .counts_col, .filter_cols_vec,
                     .group_col,
                     .unnest_col = NULL,
                     .job,
                     .base_log = 2,
                     .q = NULL) {
  prep.df <- prep_taxa_df(
    .dataframe = .dataframe,
    .key_col = {{ .key_col }},
    .unnest_col = {{ .unnest_col }},
    .filter = NULL
  )
  #----------------------------------------------------------------------------
  list.metrics <- lapply(.filter_cols_vec, function(col.i) {
    # print(col.i)
    col.i <- rlang::sym(col.i)
    taxa.vec <- prep.df %>%
      dplyr::select({{ col.i }}) %>%
      dplyr::distinct() %>%
      tidyr::drop_na() %>%
      dplyr::pull({{ col.i }}) %>%
      trimws()

    taxa.vec <- taxa.vec[nchar(taxa.vec) > 0]

    if (length(taxa.vec) == 0) {
      return(data.frame())
    }

    # length(taxa.vec)
    # str_length(taxa.vec)
    # grepl("", taxa.vec)
    taxa.df <- sapply(taxa.vec, function(taxa.i) {
      #------------------------------------------------------------------------
      if (.job == "abund") {
        vec.i <- taxa_abund(.dataframe,
          .key_col = {{ .key_col }},
          .counts_col = {{ .counts_col }},
          .filter = {{ col.i }} %in% taxa.i,
          .unnest_col = {{ .unnest_col }}
        )
      }
      #------------------------------------------------------------------------
      if (.job == "pct") {
        vec.i <- taxa_pct(.dataframe,
          .key_col = {{ .key_col }},
          .counts_col = {{ .counts_col }},
          .filter = {{ col.i }} %in% taxa.i,
          .unnest_col = {{ .unnest_col }}
        )
      }
      #------------------------------------------------------------------------
      if (.job == "rich") {
        vec.i <- taxa_rich(.dataframe,
          .key_col = {{ .key_col }},
          .filter = {{ col.i }} %in% taxa.i,
          .group_col = {{ .group_col }},
          .unnest_col = {{ .unnest_col }}
        )
      }
      #------------------------------------------------------------------------
      if (.job == "pct_rich") {
        vec.i <- taxa_pct_rich(.dataframe,
          .key_col = {{ .key_col }},
          .filter = {{ col.i }} %in% taxa.i,
          .group_col = {{ .group_col }},
          .unnest_col = {{ .unnest_col }}
        )
      }
      #------------------------------------------------------------------------
      if (.job %in% c(
        "shannon", "effective_shannon", "simpson", "invsimpson",
        "gini_simpson", "effective_simpson", "pielou",
        "margalef", "menhinick", "hill", "renyi"
      )) {
        vec.i <- taxa_div(.dataframe,
          .key_col = {{ .key_col }},
          .counts_col = {{ .counts_col }},
          .group_col = {{ .group_col }},
          .filter = {{ col.i }} %in% taxa.i,
          .job = .job,
          .base_log = .base_log,
          .q = .q,
          .unnest_col = {{ .unnest_col }}
        )
      }
      #------------------------------------------------------------------------
      return(vec.i)
    }) %>%
      as.data.frame() %>%
      dplyr::rename_all(tolower)

    if (!rlang::quo_is_null(rlang::enquo(.group_col))) {
      names(taxa.df) <- paste(.job,
                              names(taxa.df),
                              sep = "_")
    } else {
      names(taxa.df) <- paste(.job,
                              rlang::quo_name(rlang::enquo(.group_col)),
                              names(taxa.df),
                              sep = "_")
    }

    return(taxa.df)
  })
  final.df <- dplyr::bind_cols(list.metrics)
  return(final.df)
}
