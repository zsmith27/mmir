#'Probabilistic Rarefaction
#'@description Standardized subsample.
#'@param long.df Taxonomic counts arranged in a long data format.
#'@param unique.id.col The name of the column that contains a unique sampling
#'event ID.
#'@param count.col The name of the column that contains taxanomic counts.
#'@param taxon.col The name of the column that contains the taxon or taxa
#'of interest.
#'@param sample The size of the subsample.
#'@return A data frame.
#'@export


prob_rarefaction <- function(long.df, unique.id.col, count.col, taxon.col, sample) {
  unique.id.col <- rlang::enquo(unique.id.col)
  count.col <- rlang::enquo(count.col)
  taxon.col <- rlang::enquo(taxon.col)
  #----------------------------------------------------------------------------
  if (any(is.na(long.df[, quo_name(taxon.col)]))) {
    stop("The speciefied 'taxon.col' cannot contain any NAs. Select another column or fill the rows with proceeding taxonomic ranks using mmir::fill_taxa().")
  }
  #----------------------------------------------------------------------------
  wide.df <- long.df %>%
    dplyr::select(!!unique.id.col, !!taxon.col, !!count.col) %>%
    tidyr::complete(!!taxon.col, !!unique.id.col) %>%
    dplyr::mutate(!!quo_name(count.col) := if_else(is.na(!!count.col),
                                                   as.integer(0),
                                                   as.integer(!!count.col))) %>%
    dplyr::group_by(!!unique.id.col, !!taxon.col) %>%
    dplyr::summarize(!!quo_name(count.col) := sum(!!count.col)) %>%
    dplyr::group_by(!!unique.id.col) %>%
    dplyr::mutate(total = sum(!!count.col)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(total > sample) %>%
    tidyr::spread(!!taxon.col, !!count.col)

  vegan.df <- wide.df %>%
    dplyr::select(!!unique.id.col) %>%
    dplyr::mutate(rare = vegan::rarefy(wide.df[, 3:ncol(wide.df)], sample, 2)[1,])

  rich.df <- long.df %>%
    dplyr::select(!!unique.id.col) %>%
    dplyr::distinct() %>%
    dplyr::mutate(rich = taxa_rich(long.df, !!unique.id.col, rlang::UQ(count.col), !!taxon.col))

  rare.df <- dplyr::left_join(long.df, rich.df, by = dplyr::quo_name(unique.id.col)) %>%
    dplyr::left_join(vegan.df, by = dplyr::quo_name(unique.id.col)) %>%
    group_by(unique_id) %>%
    dplyr::mutate(total = sum(!!count.col),
                  rare_count = dplyr::if_else(is.na(rare),
                                              as.double(!!count.col),
                                              rlang::UQ(count.col) / total * sample),
                  rare = dplyr::if_else(is.na(rare), rich, as.integer(round(rare, 0)))) %>%
    dplyr::ungroup()

  last.value <- rare.df %>%
    dplyr::select(!!unique.id.col, !!count.col, rare, rare_count) %>%
    dplyr::group_by(!!unique.id.col) %>%
    # dplyr::mutate(row_num = dplyr::row_number(dplyr::desc(!!count.col)))
    dplyr::filter(dplyr::row_number(dplyr::desc(!!count.col)) == rare) %>%
    dplyr::select(!!unique.id.col, rare_count) %>%
    dplyr::rename(last_value = rare_count) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()

  final.df <- dplyr::left_join(rare.df, last.value, by = dplyr::quo_name(unique.id.col)) %>%
    dplyr::group_by(!!unique.id.col) %>%
    dplyr::mutate(greater_than_last = sum(rare_count > last_value),
                  last_random = rare - greater_than_last,
                  last_count = sum(rare_count == last_value),
                  #last_random = dplyr::if_else(last_random < 0, as.integer(0), last_random),
                  #last_diff = last_count - last_random,
                  rare_cat = dplyr::case_when(
                    rare_count > last_value ~ "keep",
                    rare_count == last_value ~ "last",
                    rare_count < last_value ~ "zero",
                    TRUE ~ "ERROR"
                  )) %>%
    dplyr::group_by(!!unique.id.col, rare_cat) %>%
    dplyr::mutate(rand_rank = 1:n()) %>%
    dplyr::filter(!rare_cat %in% "zero") %>%
    group_by(!!unique.id.col, rare_cat) %>%
    dplyr::filter(!(rare_cat == "last" & rand_rank %in% sample(1:unique(last_count), last_count - last_random))) %>%
    dplyr::ungroup() %>%
    dplyr::select(!!unique.id.col, !!taxon.col, rare_count) %>%
    dplyr::right_join(long.df, by = c(dplyr::quo_name(unique.id.col),
                                      dplyr::quo_name(taxon.col))) %>%
    tidyr::replace_na(list(rare_count = 0)) %>%
    dplyr::mutate(rare_count = ceiling(rare_count))

  return(final.df)

}
