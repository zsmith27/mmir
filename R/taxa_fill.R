#------------------------------------------------------------------------------
#'Fill NAs with Previous Taxonomic Rank
#'@description Fill in NA values present in the taxonomic hierarchy with the
#'previous taxonomic rank.
#'@param .dataframe Taxonomic counts arranged in a long data format.
#'@param .final_id The name of the column that contains the final taxonomic
#'ID.
#'@param .prefix A character string that will be added as a prefix to the filled
#'values separated by an underscore. The default is "unidentified" and would produce for example
#'"unidentified_ephmeroptera" as a fill value. This is important for distinguishing these filled
#'values when working with taxonomic metrics.
#'@param ... A set of columns related to taxonomic ranks where NA
#'values will be replaced with the previous taxonomic rank. It is important to
#'list these columns in the appropriate order (e.g. phylum to species). The
#'order the columns are presented is used to create the hierarchy that
#'identifies the previous taxonomic rank.
#'@return A data frame.
#'@export

taxa_fill <- function(.dataframe, .final_id, .prefix =  "unidentified", ...) {
  rank.quos <- rlang::quos(...)
  .final_id <- rlang::enquo(.final_id)

  rank.vec <- .dataframe %>%
    dplyr::select(!!!rank.quos) %>%
    names()

  final.df <- .dataframe %>%
    tidyr::gather(rank, taxon, !!!rank.quos) %>%
    dplyr::mutate(rank = factor(rank, levels = rank.vec),
                  taxon = dplyr::if_else(taxon == "",
                                  NA_character_,
                                  taxon),
                  unidentified = is.na(taxon)) %>%
    dplyr::group_by(!!.final_id) %>%
    tidyr::fill(taxon) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(taxon = dplyr::if_else(unidentified %in% TRUE,
                                         paste(.prefix,
                                               taxon,
                                               sep = "_"),
                                         taxon)) %>%
    dplyr::select(-unidentified) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      names_from = rank,
      values_from = taxon)


  return(final.df)
}
