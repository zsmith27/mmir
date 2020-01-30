#==============================================================================
# Diversity Metrics
#==============================================================================
#'Percentage of a Specified Taxon
#'@description Calculate the percentage of each sample represented by the
#'specified taxon or taxa.
#'@param .data Taxonomic counts arranged in a long data format.
#'@param .key_col The name of the column that contains a unique sampling
#'event ID.
#'@param .counts_col The name of the column that contains taxanomic counts.
#'@param .group_col The name of the column that contains the taxon or taxa
#'of interest.
#'@param .filter_vec The taxon or taxa of interest. To specify more than one taxa
#'use: c("TAXA1", "TAXA2", "TAXA3").
#'@return A numeric vector of percentages.
#'@export


taxa_div <- function(.data, .key_col, .counts_col,
                     .group_col,
                     .filter_col = NULL,
                     .filter_vec = NULL,
                     .job, .base_log, .q,
                     .unnest_col = data) {
  #------------------------------------------------------------------------------
  # Prep.
  .data_unnest <- tidyr::unnest(.data, cols = !!rlang::enquo(.unnest_col))
  .key_col <- rlang::enquo(.key_col)
  .group_col <- rlang::enquo(.group_col)
  if (!is.null(.filter_col)) .filter_col <- rlang::enquo(.filter_col)
  .counts_col <- rlang::enquo(.counts_col)
  .counts_col.name <- rlang::sym(rlang::quo_name(.counts_col))

  #------------------------------------------------------------------------------
  if (!is.null(.filter_col) && is.null(.filter_vec)) {
    paste("Specifying a '.filter_col' is only useful if you",
          "also specify a '.filter_vec' found in '.filter_col'.") %>%
      stop()
  }
  if (!is.null(.filter_vec) && is.null(.filter_col)) {
    paste("Specifying a '.filter_vec' is only useful if you",
          "also specify the '.filter_col' that contains the '.filter_vec'.") %>%
      stop()
  }

  #------------------------------------------------------------------------------
  if (!is.null(.filter_vec) && !rlang::quo_is_null(.filter_col)) {
    print("Test")

    agg.df <- prep_div(.data = .data_unnest,
                       .key_col = !!.key_col,
                       .counts_col = !!.counts_col,
                       .group_col = !!.group_col,
                       .filter_col = !!.filter_col,
                       .filter_vec = .filter_vec)
  } else {
    agg.df <- .data_unnest %>%
      dplyr::select(!!.key_col, !!.group_col, !!.counts_col) %>%
      dplyr::group_by(!!.key_col) %>%
      dplyr::mutate(total = sum(!!.counts_col)) %>%
      dplyr::ungroup()
  }
  #------------------------------------------------------------------------------
  if(.job %in% c("shannon", "effective_shannon")) {
    final.vec <- agg.df %>%
      dplyr::group_by(!!.key_col, !!.group_col, total) %>%
      dplyr::summarize(count = sum(!!.counts_col)) %>%
      dplyr::mutate(p = count / total) %>%
      dplyr::mutate(log_p = -p * log(p, .base_log)) %>%
      dplyr::group_by(!!.key_col) %>%
      dplyr::summarize(final = sum(log_p)) %>%
      original_order(.data, !!.key_col) %>%
      dplyr::mutate(final = dplyr::if_else(!is.na(final), final, as.double(0))) %>%
      dplyr::pull(final)
    if(.job == "effective_shannon") final.vec <- exp(final.vec)
  }
  #------------------------------------------------------------------------------
  if(.job %in% c("simpson", "invsimpson", "gini_simpson", "effective_simpson")) {
    final.vec <- agg.df %>%
      dplyr::group_by(!!.key_col, !!.group_col, total) %>%
      dplyr::summarize(count = sum(!!.counts_col)) %>%
      dplyr::mutate(p = count / total,
                    p = p ^ 2) %>%
      dplyr::group_by(!!.key_col) %>%
      dplyr::summarize(final = sum(p)) %>%
      original_order(.data, !!.key_col) %>%
      dplyr::mutate(final = dplyr::if_else(!is.na(final), final, as.double(0))) %>%
      dplyr::pull(final)
    if(.job == "simpson") final.vec
    if(.job == "gini_simpson") final.vec <- ifelse(final.vec > 0, 1 - final.vec, 0)
    if(.job == "invsimpson") {
      final.vec <- ifelse(final.vec == 0, 0, 1 / final.vec)
    }
    if(.job == "effective_simpson") final.vec <- 1 / (1 - final.vec)
  }
  #------------------------------------------------------------------------------
  if(.job %in% c("pielou", "margalef", "menhinick")) {
    rich.vec <- taxa_rich(.data = .data,
                          .key_col = !!.key_col,
                          .group_col = !!.group_col,
                          .filter_col = !!.filter_col,
                          .filter_vec = .filter_vec,
                          .unnest_col = !!.unnest_col)
    if(.job == "pielou") final.vec <- log10(rich.vec)
    if(.job %in% c("margalef", "menhinick")) {
      abund.vec <- taxa_abund(.data = .data,
                            .key_col = !!.key_col,
                            .counts_col = !!.counts_col,
                            .filter_col = !!.filter_col,
                            .filter_vec = .filter_vec,
                            .unnest_col = !!.unnest_col)
      if(.job == "margalef") final.vec <- (rich.vec - 1) / log10(abund.vec)
      if(.job == "menhinick") final.vec <- rich.vec / sqrt(abund.vec)
    }
  }
  #------------------------------------------------------------------------------
  if(.job %in% c("hill", "renyi")) {
    final.vec <- agg.df %>%
      dplyr::group_by(!!.key_col, !!.group_col, total) %>%
      dplyr::summarize(.counts_col = sum(!!.counts_col)) %>%
      dplyr::mutate(p = .counts_col / total,
                    p = p ^ .q) %>%
      dplyr::group_by(!!.key_col) %>%
      dplyr::summarize(final = sum(p)) %>%
      original_order(.data, !!.key_col) %>%
      dplyr::pull(final)
    if(.job == "hill") final.vec <- final.vec ^ (1 / (1 - .q))
    if(.job == "renyi") final.vec <- (1 / (1 - .q)) * ln(final.vec)
  }
  #------------------------------------------------------------------------------
  return(final.vec)
}


prep_div <- function(.data, .key_col, .counts_col, .group_col,
                     .filter_col = NULL, .filter_vec = NULL) {
  .key_col <- rlang::enquo(.key_col)
  .group_col <- rlang::enquo(.group_col)
  .filter_col <- rlang::enquo(.filter_col)
  .counts_col <- rlang::enquo(.counts_col)
  .counts_col_name <- rlang::sym(rlang::quo_name(.counts_col))


  if(!is.vector(.filter_vec)) stop("The '.filter_vec' object must be a vector.")

  long.sub <- tidyr::complete(.data, !!!syms(quo_name(.key_col)), !!!syms(quo_name(.filter_col)))
  long.sub <- long.sub %>%
    dplyr::mutate(!!.counts_col_name := dplyr::if_else(is.na(!!.counts_col), 0, as.double(!!.counts_col))) %>%
    dplyr::filter((!!.filter_col) %in% .filter_vec)# %>%
  # dplyr::rename(quo_name(.counts_col) = count)

  agg.df <- long.sub %>%
    # dplyr::select(!!.key_col,
    #               (!!.filter_col),
    #               !!.group_col, !!.counts_col) %>%
    dplyr::group_by(!!.key_col) %>%
    dplyr::mutate(total = sum(!!.counts_col)) %>%
    dplyr::ungroup()

  return(agg.df)
}

# test <- taxa_div(onondaga,
#          .key_col = unique_id,
#          .counts_col = reporting_value,
#          .filter_col = order,
#          .group_col = genus,
#          .filter_vec = c("ephemeroptera", "plecoptera", "trichoptera"),
#          .job = "shannon", .base_log = 2)
