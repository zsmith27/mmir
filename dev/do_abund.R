long.df <- onondaga
count.col <- rlang::quo(reporting_value)
taxon.col <- rlang::quo(order)
taxon <- "ephemeroptera"
exclusion.col = NULL
exclusion.vec = NULL

taxa_abund2 <- function(long.df, count.col, taxon.col, taxon = NULL,
                       exclusion.col = NULL, exclusion.vec = NULL) {

# prep --------------------------------------------------------------------
  taxon.col <- rlang::enquo(taxon.col)
  count.col <- rlang::enquo(count.col)
  exclusion.col <- rlang::enquo(exclusion.col)
  #----------------------------------------------------------------------------
  if (rlang::quo_is_null(exclusion.col)) {
    # Aggregate taxonomic counts at the specified taxonomic levels.
    taxa.counts <- long.df %>%
      dplyr::select(!!taxon.col, !!count.col)
  } else {
    taxa.counts <- long.df %>%
      dplyr::select(!!taxon.col,
                    !!count.col,
                    !!exclusion.col) %>%
      dplyr::filter(!rlang::UQ(exclusion.col) %in% exclusion.vec)
  }
  #----------------------------------------------------------------------------
  # Calculate the abundance of the specified taxon.
  if (is.null(taxon)) {
    final.vec <- taxa.counts %>%
      dplyr::summarize(abund = sum(!!count.col)) %>%
      dplyr::pull(abund)
  } else {
    final.vec <- taxa.counts %>%
      dplyr::summarize(abund = sum((!!count.col)[(!!taxon.col) %in% taxon])) %>%
      dplyr::mutate(abund = as.numeric(abund),
                    abund = dplyr::if_else(!is.na(abund), abund, as.double(0))) %>%
      dplyr::pull(abund)
  }
  #----------------------------------------------------------------------------
  return(final.vec)
}


abund.df2 <- onondaga %>%
  group_by(unique_id) %>%
  do(abund_ephemeroptera = taxa_abund2(.,
                                       # unique.id.col = unique_id,
                                       count.col = reporting_value,
                                       taxon.col = order,
                                       taxon = "ephemeroptera"),
     abund_ept = taxa_abund2(.,
                             # unique.id.col = unique_id,
                             count.col = reporting_value,
                             taxon.col = order,
                             taxon = c("ephemeroptera", "plecoptera", "trichoptera"))
  )
