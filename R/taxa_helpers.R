
#------------------------------------------------------------------------------

prep_taxa_df <- function(.dataframe, .key_col, .unnest_col, .filter) {
  if (!rlang::quo_is_null(rlang::enquo(.unnest_col))) {
    .dataframe_unnest <- tidyr::unnest(.dataframe,
      cols = {{ .unnest_col }}
    )
  } else {
    .dataframe_unnest <- .dataframe
  }
  #----------------------------------------------------------------------------
  # Apply specified filter when not NULL
  if (!rlang::quo_is_null(rlang::enquo(.filter))) {
    .dataframe_final <- .dataframe_unnest %>%
      dplyr::filter({{ .filter }}) %>%
      original_order(.dataframe_unnest, {{ .key_col }})
  } else {
    .dataframe_final <- .dataframe_unnest
  }

  return(.dataframe_final)
}

#------------------------------------------------------------------------------
#' Return the Data Frame to the Order Orginally Provided

original_order <- function(.mod_data, .org_data, .key_col) {
  final.df <- .org_data %>%
    dplyr::select({{ .key_col }}) %>%
    dplyr::distinct() %>%
    dplyr::left_join(.mod_data, by = rlang::quo_name(rlang::enquo(.key_col)))

  return(final.df)
}
