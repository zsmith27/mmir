#' @title Sensitivity: Discrimination Efficiency (DE)
#' @description FUNCTION_DESCRIPTION
#' @param metrics.df PARAM_DESCRIPTION
#' @param condition.col PARAM_DESCRIPTION
#' @param ref.cond PARAM_DESCRIPTION
#' @param deg.cond PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarize}},\code{\link[dplyr]{ungroup}},\code{\link[dplyr]{select}},\code{\link[dplyr]{full_join}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{case_when}},\code{\link[dplyr]{left_join}},\code{\link[dplyr]{distinct}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{desc}}

#'  \code{\link[rlang]{quos}},\code{\link[rlang]{enquo}}

#'  \code{\link[tidyr]{gather}}
#' @rdname sensitivity_de
#' @export
#' @importFrom dplyr filter group_by summarize ungroup select full_join mutate case_when left_join distinct arrange desc
#' @importFrom rlang quos enquo
#' @importFrom tidyr gather

sensitivity_de <- function(metrics.df, condition.col, ref.cond, deg.cond, ...){
  metric.cols <- rlang::quos(...)
  condition.col <- rlang::enquo(condition.col)
  #----------------------------------------------------------------------------
  metrics.long <- metrics.df %>%
    tidyr::gather(metric, value, !!!metric.cols)
  #----------------------------------------------------------------------------
  ref.df <- metrics.long %>%
    dplyr::filter((!!condition.col) %in% ref.cond) %>%
    dplyr::group_by(!!condition.col, metric) %>%
    dplyr::summarize(quant_25 = quantile(value, 0.25, na.rm = TRUE),
                     quant_50 = median(value, na.rm = TRUE),
                     quant_75 = quantile(value, 0.75, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-(!!condition.col))
  #----------------------------------------------------------------------------
  deg.df <- metrics.long %>%
    dplyr::filter((!!condition.col) %in% deg.cond) %>%
    dplyr::group_by(!!condition.col, metric) %>%
    dplyr::summarize(deg_quant_50 = median(value, na.rm = TRUE),
                     deg_count = n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-(!!condition.col))
  #----------------------------------------------------------------------------
  metrics.info <- dplyr::full_join(ref.df, deg.df, by = "metric") %>%
    dplyr::mutate(disturbance = dplyr::case_when(
      quant_50 > deg_quant_50 ~ "decrease",
      quant_50 < deg_quant_50 ~ "increase",
      TRUE ~ "equal"
    ))
  #----------------------------------------------------------------------------
  final.df <- dplyr::left_join(metrics.long, metrics.info, by = "metric") %>%
    dplyr::filter((!!condition.col) %in% deg.cond) %>%
    dplyr::group_by(metric) %>%
    dplyr::mutate(de_sensitivity = dplyr::case_when(
      disturbance == "decrease" ~ sum(value < quant_25) / unique(deg_count) * 100,
      disturbance == "increase" ~ sum(value > quant_75) / unique(deg_count) * 100,
      TRUE ~ as.double(0)
    )) %>%
    dplyr::select(metric, disturbance, de_sensitivity) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::desc(de_sensitivity))

  return(final.df)
}
