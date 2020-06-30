#==============================================================================
#Metric Sensitivity
#==============================================================================
#' @title Spread Condition
#' @description FUNCTION_DESCRIPTION
#' @param .dataframe PARAM_DESCRIPTION
#' @param .metric_col PARAM_DESCRIPTION
#' @param .condition_col PARAM_DESCRIPTION
#' @param .value_col PARAM_DESCRIPTION
#' @param .reference PARAM_DESCRIPTION
#' @param .degraded PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rlang]{enquo}},\code{\link[rlang]{sym}},\code{\link[rlang]{quo_name}}
#' @rdname spread_condition
#' @export
#' @importFrom rlang enquo sym quo_name
#' @importFrom magrittr "%>%"

spread_condition  <- function(.dataframe, .metric_col, .condition_col, .value_col, .reference, .degraded) {
  .metric_col <- rlang::enquo(.metric_col)
  .value_col <- rlang::enquo(.value_col)
  .condition_col <- rlang::enquo(.condition_col)
  ref.col.name <- rlang::sym(paste("ref", rlang::quo_name(.value_col), sep = "_"))
  deg.col.name <- rlang::sym(paste("deg", rlang::quo_name(.value_col), sep = "_"))

  .dataframe %>%
    dplyr::select(!!.metric_col, !!.condition_col, !!.value_col) %>%
    tidyr::spread(!!.condition_col, !!.value_col) %>%
    dplyr::rename(!!ref.col.name := !!rlang::sym(.reference),
                  !!deg.col.name := !!rlang::sym(.degraded))
}

#==============================================================================
#' @title Prepare Sensitivity
#' @description FUNCTION_DESCRIPTION
#' @param .dataframe PARAM_DESCRIPTION
#' @param .metric_col PARAM_DESCRIPTION
#' @param .value_col PARAM_DESCRIPTION
#' @param .condition_col PARAM_DESCRIPTION
#' @param .reference PARAM_DESCRIPTION
#' @param .degraded PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rlang]{enquo}},\code{\link[rlang]{quo_name}}
#' @rdname prep_sensitivity
#' @export

prep_sensitivity <- function(.dataframe, .metric_col, .value_col, .condition_col, .reference, .degraded) {
  .metric_col <- rlang::enquo(.metric_col)
  .value_col <- rlang::enquo(.value_col)
  .condition_col <- rlang::enquo(.condition_col)

  metrics.df <- .dataframe %>%
    dplyr::select(!!.metric_col, !!.value_col, !!.condition_col) %>%
    dplyr::filter((!!.condition_col) %in% c(.reference, .degraded))

  metrics.df <- metrics.df %>%
    dplyr::group_by(!!.metric_col, !!.condition_col) %>%
    dplyr::summarize(values = list(!!.value_col),
                     total = length(!!.value_col),
                     quantiles = list(quantile(!!.value_col, probs = seq(0, 1, by = 0.01), na.rm = TRUE)),
                     quant25 = quantile(!!.value_col, 0.25, na.rm = TRUE),
                     median = quantile(!!.value_col, 0.50, na.rm = TRUE),
                     quant75 = quantile(!!.value_col, 0.75, na.rm = TRUE)) %>%
    dplyr::ungroup()


  final.df <- dplyr::left_join(
    spread_condition(metrics.df, !!.metric_col, !!.condition_col, values, .reference, .degraded),
    spread_condition(metrics.df, !!.metric_col, !!.condition_col, total, .reference, .degraded),
    by = rlang::quo_name(.metric_col)) %>%
    dplyr::left_join(spread_condition(metrics.df, !!.metric_col, !!.condition_col, median, .reference, .degraded),
                     by = rlang::quo_name(.metric_col)) %>%
    dplyr::left_join(spread_condition(metrics.df, !!.metric_col, !!.condition_col, quant25, .reference, .degraded),
                     by = rlang::quo_name(.metric_col)) %>%
    dplyr::left_join(spread_condition(metrics.df, !!.metric_col, !!.condition_col, quant75, .reference, .degraded),
                     by = rlang::quo_name(.metric_col)) %>%
    dplyr::left_join(spread_condition(metrics.df, !!.metric_col, !!.condition_col, quantiles, .reference, .degraded),
                     by = rlang::quo_name(.metric_col))

  final.df <- final.df %>%
    dplyr::mutate(disturbance = dplyr::case_when(
      ref_median > deg_median ~ "decrease",
      ref_median < deg_median ~ "increase",
      ref_median == deg_median ~ "equal",
      TRUE ~ "ERROR"
    ),
    total = ref_total + deg_total)

  return(final.df)
}
#==============================================================================
#' @title Barbour et. al 1996
#' @description FUNCTION_DESCRIPTION
#' @param prep.sensitivity PARAM_DESCRIPTION
#' @param .metric_col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rlang]{enquo}}
#' @rdname barbour
#' @export
#' @importFrom rlang enquo

barbour <- function(prep.sensitivity, .metric_col) {
  .metric_col <- rlang::enquo(.metric_col)

  prep.sensitivity %>%
    dplyr::mutate(barbour = dplyr::case_when(
      ref_median <= deg_quant75 & ref_median >= deg_quant25 &
        deg_median <= ref_quant75 & deg_median >= ref_quant25 ~ 0,
      ref_median <= deg_quant75 & ref_median >= deg_quant25  ~ 1,
      deg_median <= ref_quant75 & deg_median >= ref_quant25 ~ 1,
      ref_quant25 <= deg_quant75 & ref_quant25 >= deg_quant25 ~ 2,
      ref_quant75 <= deg_quant75 & ref_quant75 >= deg_quant25 ~ 2,
      TRUE ~ 3)) %>%
    dplyr::select(!!.metric_col, barbour)
}
#==============================================================================
#' @title Discrimination Efficiency
#' @description FUNCTION_DESCRIPTION
#' @param prep.sensitivity PARAM_DESCRIPTION
#' @param .metric_col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rlang]{enquo}}
#' @rdname de_orginal
#' @export
#' @importFrom rlang enquo

de_orginal <- function(prep.sensitivity, .metric_col) {
  .metric_col <- rlang::enquo(.metric_col)

  prep.sensitivity %>%
    dplyr::group_by(!!.metric_col) %>%
    dplyr::mutate(de = dplyr::case_when(
      disturbance == "decrease" ~ sum((unlist(deg_values) < ref_quant25)) / deg_total * 100,
      disturbance == "equal" ~ as.double(0),
      disturbance == "increase"  ~ sum(unlist(deg_values) > ref_quant75) / deg_total * 100,
      TRUE ~ as.double(10 ^ 6)
    ),
    de_thresh = dplyr::case_when(
      disturbance == "decrease" ~ ref_quant25,
      disturbance == "equal" ~ as.double(0),
      disturbance == "increase"  ~ ref_quant75,
      TRUE ~ as.double(10 ^ 6)
    )) %>%
    dplyr::ungroup() %>%
    dplyr::select(!!.metric_col, de_thresh, de)
}
#==============================================================================
#' @title Test Sensitivity Thresholds
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param less.than.col PARAM_DESCRIPTION
#' @param greater.than.col PARAM_DESCRIPTION
#' @param thresh PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rlang]{enquo}}
#' @rdname test_thresh
#' @export
#' @importFrom rlang enquo

test_thresh <- function(x, less.than.col, greater.than.col, thresh) {
  less.than.col <- rlang::enquo(less.than.col)
  greater.than.col <- rlang::enquo(greater.than.col)

  final.df <- x %>%
    dplyr::mutate(group1_pct = sum((unlist(!!less.than.col) < thresh)) / length(unlist(!!less.than.col)) * 100,
                  group2_pct = sum((unlist(!!greater.than.col) > thresh)) / length(unlist(!!greater.than.col)) * 100,
                  bal_factor = abs(group1_pct - group2_pct),
                  bde = (sum(group1_pct, group2_pct) / 2),
                  bde_bf = bde - bal_factor,
                  bde_thresh = thresh) %>%
    dplyr::ungroup() #%>%
  #select(bde_thresh, bde)

  list(final.df$bde_thresh, final.df$bde_bf, final.df$bde)
}
#==============================================================================
#' @title Select A Sensitivity Threshold
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param thresh.col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rlang]{enquo}}
#'  \code{\link[purrr]{map}}
#' @rdname select_thresh
#' @export
#' @importFrom rlang enquo
#' @importFrom purrr map

select_thresh <- function(x, thresh.col) {
  thresh.col <- rlang::enquo(thresh.col)
  test4 <- x %>%
    #head(1) %>%
    dplyr::mutate(bde_list = list(purrr::map(unlist(!!thresh.col), function(thresh.i) {
      if (disturbance == "decrease") {
        test_thresh(x = ., less.than.col = deg_values, greater.than.col = ref_values, thresh = thresh.i)
      } else if(disturbance == "increase") {
        test_thresh(x = ., less.than.col = ref_values, greater.than.col = deg_values, thresh = thresh.i)
      } else if(disturbance == "equal") list(ref_median, 50.00, 50.00)
    }))) %>%
    dplyr::mutate(bde_vec = list(purrr::map_dbl(bde_list[1][[1]],`[[`, 2))) %>%
    dplyr::mutate(sel_vec = bde_list[1][[1]][median(which(unlist(bde_vec) == max(unlist(bde_vec))))])

  test4$sel_vec
}

#==============================================================================
#' @title BDE
#' @description FUNCTION_DESCRIPTION
#' @param prep.sensitivity PARAM_DESCRIPTION
#' @param .metric_col PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rlang]{enquo}}
#'  \code{\link[purrr]{map}}
#' @rdname bde
#' @export
#' @importFrom rlang enquo
#' @importFrom purrr map

bde <- function(prep.sensitivity, .metric_col) {
  .metric_col <- rlang::enquo(.metric_col)

  prep.sensitivity %>%
    dplyr::group_by(!!.metric_col) %>%
    dplyr::mutate(pthresh = list(seq(ref_median, deg_median,
                                     by = -1* ((ref_median - deg_median) / (25))))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(sel_vec = purrr::map(seq(nrow(.)), function(row.i) {
      dplyr::slice(., row.i) %>%
        select_thresh(pthresh)
    })) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(bde_thresh = unlist(sel_vec)[1],
                  bde = unlist(sel_vec)[3]) %>%
    dplyr::ungroup() %>%
    dplyr::select(!!.metric_col, bde_thresh, bde)
}
#==============================================================================
#' @title Metric Sensitivity
#' @description FUNCTION_DESCRIPTION
#' @param .dataframe PARAM_DESCRIPTION
#' @param .metric_col PARAM_DESCRIPTION
#' @param .value_col PARAM_DESCRIPTION
#' @param .condition_col PARAM_DESCRIPTION
#' @param .reference PARAM_DESCRIPTION
#' @param .degraded PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rlang]{enquo}},\code{\link[rlang]{quo_name}}
#' @rdname sensitivity
#' @export
#' @importFrom rlang enquo quo_name

sensitivity <- function(.dataframe, .metric_col, .value_col,
                        .condition_col, .reference, .degraded) {
  .metric_col <- rlang::enquo(.metric_col)
  .value_col <- rlang::enquo(.value_col)
  .condition_col <- rlang::enquo(.condition_col)

  prep.df <- prep_sensitivity(.dataframe, !!.metric_col, !!.value_col, !!.condition_col, .reference, .degraded)

  final.df <- dplyr::left_join(dplyr::select(prep.df, !!.metric_col, disturbance),
                               barbour(prep.df, !!.metric_col),
                               by = rlang::quo_name(.metric_col)) %>%
    dplyr::left_join(de_orginal(prep.df, !!.metric_col),
                     by = rlang::quo_name(.metric_col)) %>%
    dplyr::left_join(bde(prep.df, !!.metric_col),
                     by = rlang::quo_name(.metric_col))

  final.df <- dplyr::arrange(final.df, desc(bde))
  return(final.df)
}
