
#------------------------------------------------------------------------------
#'Return the Data Frame to the Order Orginally Provided
#'@description Return the data frame to the order orginally provided.
#'@param .data The data frame
#'@param .unnest_col Name the nested column
#'@param .filter supply a logical statement
#'@param .key_col The name of the column that contains a unique sampling.
#'@return A data frame.
#'@export

prep_taxa_df <- function(.data, .key_col, .unnest_col, .filter){
  if (!rlang::quo_is_null(rlang::enquo(.unnest_col))) {
    .data_unnest <- tidyr::unnest(.data,
                                  cols = {{.unnest_col}})
  } else {
    .data_unnest <- .data
  }
  #----------------------------------------------------------------------------
  # Apply specified filter when not NULL
  if (!rlang::quo_is_null(rlang::enquo(.filter))) {
    .data_final <- .data_unnest %>%
      dplyr::filter({{.filter}}) %>%
      original_order(.data_unnest, {{.key_col}})
  } else {
    .data_final <- .data_unnest
  }

  return(.data_final)
}

#------------------------------------------------------------------------------
#'Return the Data Frame to the Order Orginally Provided
#'@description Return the data frame to the order orginally provided.
#'@param mod.df The data frame altered during metric calculation.
#'@param original.df The orginal data frame, and therefore the original order of the unique.id.col.
#'@param unique.id.col The name of the column that contains a unique sampling.
#'@return A data frame.
#'@export

original_order <- function(.mod_data, .org_data, .key_col) {
  final.df <- .org_data %>%
    dplyr::select({{.key_col}}) %>%
    dplyr::distinct() %>%
    dplyr::left_join(.mod_data, by = rlang::quo_name(rlang::enquo(.key_col)))

  return(final.df)
}
