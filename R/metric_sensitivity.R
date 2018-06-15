#==============================================================================
#Metric Sensitivity
#==============================================================================
#' @title Spread Condition
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param metric.col PARAM_DESCRIPTION
#' @param condition.col PARAM_DESCRIPTION
#' @param value.col PARAM_DESCRIPTION
#' @param ref.cond PARAM_DESCRIPTION
#' @param deg.cond PARAM_DESCRIPTION
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

spread_condition  <- function(x, metric.col, condition.col, value.col, ref.cond, deg.cond) {
  metric.col <- rlang::enquo(metric.col)
  value.col <- rlang::enquo(value.col)
  condition.col <- rlang::enquo(condition.col)
  ref.col.name <- rlang::sym(paste("ref", rlang::quo_name(value.col), sep = "_"))
  deg.col.name <- rlang::sym(paste("deg", rlang::quo_name(value.col), sep = "_"))

  x %>%
    select(!!metric.col, !!condition.col, !!value.col) %>%
    spread(!!condition.col, !!value.col) %>%
    rename(!!ref.col.name := !!rlang::sym(ref.cond),
           !!deg.col.name := !!rlang::sym(deg.cond))
}

#==============================================================================
#' @title Prepare Sensitivity
#' @description FUNCTION_DESCRIPTION
#' @param metrics.long PARAM_DESCRIPTION
#' @param metric.col PARAM_DESCRIPTION
#' @param value.col PARAM_DESCRIPTION
#' @param condition.col PARAM_DESCRIPTION
#' @param ref.cond PARAM_DESCRIPTION
#' @param deg.cond PARAM_DESCRIPTION
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

prep_sensitivity <- function(metrics.long, metric.col, value.col, condition.col, ref.cond, deg.cond) {
  metric.col <- rlang::enquo(metric.col)
  value.col <- rlang::enquo(value.col)
  condition.col <- rlang::enquo(condition.col)

  metrics.df <- metrics.long %>%
    select(!!metric.col, !!value.col, !!condition.col) %>%
    filter((!!condition.col) %in% c(ref.cond, deg.cond))

  metrics.df <- metrics.df %>%
    group_by(!!metric.col, !!condition.col) %>%
    summarize(values = list(!!value.col),
              total = length(!!value.col),
              quantiles = list(quantile(!!value.col, probs = seq(0, 1, by = 0.01), na.rm = TRUE)),
              quant25 = quantile(!!value.col, 0.25, na.rm = TRUE),
              median = quantile(!!value.col, 0.50, na.rm = TRUE),
              quant75 = quantile(!!value.col, 0.75, na.rm = TRUE)) %>%
    ungroup()


  final.df <- left_join(
    spread_condition(metrics.df, !!metric.col, !!condition.col, values, ref.cond, deg.cond),
    spread_condition(metrics.df, !!metric.col, !!condition.col, total, ref.cond, deg.cond),
    by = rlang::quo_name(metric.col)) %>%
    left_join(spread_condition(metrics.df, !!metric.col, !!condition.col, median, ref.cond, deg.cond),
              by = rlang::quo_name(metric.col)) %>%
    left_join(spread_condition(metrics.df, !!metric.col, !!condition.col, quant25, ref.cond, deg.cond),
              by = rlang::quo_name(metric.col)) %>%
    left_join(spread_condition(metrics.df, !!metric.col, !!condition.col, quant75, ref.cond, deg.cond),
              by = rlang::quo_name(metric.col)) %>%
    left_join(spread_condition(metrics.df, !!metric.col, !!condition.col, quantiles, ref.cond, deg.cond),
              by = rlang::quo_name(metric.col))

  final.df <- final.df %>%
    mutate(disturbance = case_when(
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
#' @param metric.col PARAM_DESCRIPTION
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

barbour <- function(prep.sensitivity, metric.col) {
  metric.col <- rlang::enquo(metric.col)

  prep.sensitivity %>%
    mutate(barbour = case_when(
      ref_median <= deg_quant75 & ref_median >= deg_quant25 &
        deg_median <= ref_quant75 & deg_median >= ref_quant25 ~ 0,
      ref_median <= deg_quant75 & ref_median >= deg_quant25  ~ 1,
      deg_median <= ref_quant75 & deg_median >= ref_quant25 ~ 1,
      ref_quant25 <= deg_quant75 & ref_quant25 >= deg_quant25 ~ 2,
      ref_quant75 <= deg_quant75 & ref_quant75 >= deg_quant25 ~ 2,
      TRUE ~ 3)) %>%
    select(!!metric.col, barbour)
}
#==============================================================================
#' @title Discrimination Efficiency
#' @description FUNCTION_DESCRIPTION
#' @param prep.sensitivity PARAM_DESCRIPTION
#' @param metric.col PARAM_DESCRIPTION
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

de_orginal <- function(prep.sensitivity, metric.col) {
  metric.col <- rlang::enquo(metric.col)

  prep.sensitivity %>%
    group_by(!!metric.col) %>%
    mutate(de = case_when(
      disturbance == "decrease" ~ sum((unlist(deg_values) < ref_quant25)) / deg_total * 100,
      disturbance == "equal" ~ as.double(0),
      disturbance == "increase"  ~ sum(unlist(deg_values) > ref_quant75) / deg_total * 100,
      TRUE ~ as.double(10 ^ 6)
    ),
    de_thresh = case_when(
      disturbance == "decrease" ~ ref_quant25,
      disturbance == "equal" ~ as.double(0),
      disturbance == "increase"  ~ ref_quant75,
      TRUE ~ as.double(10 ^ 6)
    )) %>%
    ungroup() %>%
    select(!!metric.col, de_thresh, de)
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
    mutate(group1_pct = sum((unlist(!!less.than.col) < thresh)) / length(unlist(!!less.than.col)) * 100,
           group2_pct = sum((unlist(!!greater.than.col) > thresh)) / length(unlist(!!greater.than.col)) * 100,
           bal_factor = abs(group1_pct - group2_pct),
           bde = (sum(group1_pct, group2_pct) / 2),
           bde_bf = bde - bal_factor,
           bde_thresh = thresh) %>%
    ungroup() #%>%
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
    mutate(bde_list = list(purrr::map(unlist(!!thresh.col), function(thresh.i) {
      if (disturbance == "decrease") {
        test_thresh(x = ., less.than.col = deg_values, greater.than.col = ref_values, thresh = thresh.i)
      } else if(disturbance == "increase") {
        test_thresh(x = ., less.than.col = ref_values, greater.than.col = deg_values, thresh = thresh.i)
      } else if(disturbance == "equal") list(ref_median, 50.00, 50.00)
    }))) %>%
    mutate(bde_vec = list(map_dbl(bde_list[1][[1]],`[[`, 2))) %>%
    mutate(sel_vec = bde_list[1][[1]][median(which(unlist(bde_vec) == max(unlist(bde_vec))))])

  test4$sel_vec
}

#==============================================================================
#' @title BDE
#' @description FUNCTION_DESCRIPTION
#' @param prep.sensitivity PARAM_DESCRIPTION
#' @param metric.col PARAM_DESCRIPTION
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

bde <- function(prep.sensitivity, metric.col) {
  metric.col <- rlang::enquo(metric.col)

  prep.sensitivity %>%
    group_by(!!metric.col) %>%
    mutate(pthresh = list(seq(ref_median, deg_median,
                              by = -1* ((ref_median - deg_median) / (25))))) %>%
    ungroup() %>%
    mutate(sel_vec = purrr::map(seq(nrow(.)), function(row.i) {
      slice(., row.i) %>%
        select_thresh(pthresh)
    })) %>%
    rowwise() %>%
    mutate(bde_thresh = unlist(sel_vec)[1],
           bde = unlist(sel_vec)[3]) %>%
    ungroup() %>%
    select(!!metric.col, bde_thresh, bde)
}
#==============================================================================
#' @title Metric Sensitivity
#' @description FUNCTION_DESCRIPTION
#' @param metrics.long PARAM_DESCRIPTION
#' @param metric.col PARAM_DESCRIPTION
#' @param value.col PARAM_DESCRIPTION
#' @param condition.col PARAM_DESCRIPTION
#' @param ref.cond PARAM_DESCRIPTION
#' @param deg.cond PARAM_DESCRIPTION
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

sensitivity <- function(metrics.long, metric.col, value.col,
                        condition.col, ref.cond, deg.cond) {
  metric.col <- rlang::enquo(metric.col)
  value.col <- rlang::enquo(value.col)
  condition.col <- rlang::enquo(condition.col)

  prep.df <- prep_sensitivity(metrics.long, !!metric.col, !!value.col, !!condition.col, ref.cond, deg.cond)

  final.df <- left_join(select(prep.df, !!metric.col, disturbance),
                        barbour(prep.df, !!metric.col),
                        by = rlang::quo_name(metric.col)) %>%
    left_join(de_orginal(prep.df, !!metric.col),
              by = rlang::quo_name(metric.col)) %>%
    left_join(bde(prep.df, !!metric.col),
              by = rlang::quo_name(metric.col))

  final.df <- arrange(final.df, desc(bde))
  return(final.df)
}
