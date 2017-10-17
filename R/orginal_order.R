#'Return the Data Frame to the Order Orginally Provided
#'@description Return the data frame to the order orginally provided.
#'@param mod.df The data frame altered during metric calculation.
#'@param original.df The orginal data frame, and therefore the original order of the unique.id.col.
#'@param unique.id.col The name of the column that contains a unique sampling.
#'@return A data frame.
#'@export

original_order <- function(mod.df, original.df, unique.id.col) {
  unique.id.col <- rlang::enquo(unique.id.col)

  final.df <- original.df %>%
    dplyr::select(!!unique.id.col) %>%
    dplyr::distinct() %>%
    dplyr::left_join(mod.df, by = rlang::quo_name(unique.id.col))

  return(final.df)
}
