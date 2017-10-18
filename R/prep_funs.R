#'Clean Up Data Frames
#'@description Performs some data frame clean up that I have found to
#'reduce errors and make it easier to work with the data frame. All
#'of the column names are converted to all lower case and any leading
#'or trailing white space is removed. Similarly, all character columns
#'are converted to all lower case and any leading or trailing white space
#'is removed.
#'@param x A data frame or tibble.
#'@return A data frame.
#'@export
clean_df <- function(x) {
  if (!is.data.frame(x)) stop("'x' must be a data frame or tibble")
  final.df <- x %>%
    dplyr::rename_all(tolower) %>%
    dplyr::rename_all(stringr::str_trim) %>%
    dplyr::mutate_if(is.character, tolower) %>%
    dplyr::mutate_if(is.character, stringr::str_trim)
  return(final.df)
}

#------------------------------------------------------------------------------
#'Check for Duplicates
#'@description Check the data frame for duplicates based on the specified
#'column(s).
#'@param x A data frame or tibble.
#'@param ... A column or set of columns to aggreate by and check for duplicates.
#'@return A data frame containing the counts of the variables in the specified
#'column(s). Values greater than one indicate duplicates.
#'@export
check_dups <- function(x, ...) {
  group_by <- rlang::quos(...)
  final.df <- x %>%
    dplyr::group_by(!!!group_by) %>%
    dplyr::summarize(count = n()) %>%
    dplyr::arrange(dplyr::desc(count))
  return(final.df)
}

#------------------------------------------------------------------------------
#'Fill NAs with Previous Taxonomic Rank
#'@description Fill in NA values present in the taxonomic hierarchy with the
#'previous taxonomic rank.
#'@param long.df Taxonomic counts arranged in a long data format.
#'@param final.id.col The name of the column that contains the final taxonomic
#'ID.
#'@param ... A set of columns related to taxonomic ranks where NA
#'values will be replaced with the previous taxonomic rank. It is important to
#'list these columns in the appropriate order (e.g. phylum to species). The
#'order the columns are presented is used to create the hierarchy that
#'identifies the previous taxonomic rank.
#'@return A data frame.
#'@export

fill_taxa <- function(long.df, final.id.col, ...) {
  rank.quos <- rlang::quos(...)
  final.id.col <- rlang::enquo(final.id.col)

  rank.vec <- long.df %>%
    select(!!!rank.quos) %>%
    names()

  final.df <- long.df %>%
    tidyr::gather(rank, taxon, !!!rank.quos) %>%
    dplyr::mutate(rank = factor(rank, levels = rank.vec),
                  taxon = if_else(taxon == "", as.character(NA), taxon)) %>%
    dplyr::group_by(!!final.id.col) %>%
    tidyr::fill(taxon)  %>%
    dplyr::ungroup() %>%
    tidyr::spread(rank, taxon)

  return(final.df)
}
