#'Return the Data Frame to the Order Orginally Provided
#'@description Return the data frame to the order orginally provided.
#'@param long.df Taxonomic counts arranged in a long data format.
#'@param unique.id.col The name of the column that contains a unique sampling
#'@return A data frame.
#'@export

original_order <- function(long.df, unique.id.col) {
  unique.id.col <- rlang::enquo(unique.id.col)

  long.df <- long.df %>%
    dplyr::rename(unique_id = !!unique.id.col)

  final.df <- long.df %>%
    dplyr::select(unique_id) %>%
    dplyr::distinct() %>%
    dplyr::right_join(long.df, by = "unique_id")

  return(final.df)
}
