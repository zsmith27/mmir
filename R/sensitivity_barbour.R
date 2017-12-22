#' @title Sensitivity: Barbour et al. (1996) Method
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
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarize}},\code{\link[dplyr]{ungroup}},\code{\link[dplyr]{select}},\code{\link[dplyr]{full_join}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{case_when}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{desc}}

#'  \code{\link[rlang]{quos}},\code{\link[rlang]{enquo}}

#'  \code{\link[tidyr]{gather}}
#' @rdname sensitivity_barbour
#' @export
#' @importFrom dplyr filter group_by summarize ungroup select full_join mutate case_when arrange desc
#' @importFrom rlang quos enquo
#' @importFrom tidyr gather

sensitivity_barbour <- function(metrics.df, condition.col, ref.cond, deg.cond, ...){
  metric.cols <- rlang::quos(...)
  condition.col <- rlang::enquo(condition.col)
  #----------------------------------------------------------------------------
  metrics.long <- metrics.df %>%
    tidyr::gather(metric, value, !!!metric.cols)
  #----------------------------------------------------------------------------
  ref.df <- metrics.long %>%
    dplyr::filter((!!condition.col) %in% ref.cond) %>%
    dplyr::group_by(!!condition.col, metric) %>%
    dplyr::summarize(ref_quant_25 = quantile(value, 0.25, na.rm = TRUE),
                     ref_quant_50 = quantile(value, 0.50, na.rm = TRUE),
                     ref_quant_75 = quantile(value, 0.75, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-(!!condition.col))
  #----------------------------------------------------------------------------
  deg.df <- metrics.long %>%
    dplyr::filter((!!condition.col) %in% deg.cond) %>%
    dplyr::group_by(!!condition.col, metric) %>%
    dplyr::summarize(deg_quant_25 = quantile(value, 0.25, na.rm = TRUE),
                     deg_quant_50 = quantile(value, 0.50, na.rm = TRUE),
                     deg_quant_75 = quantile(value, 0.75, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-(!!condition.col))
  #----------------------------------------------------------------------------
  # Scoring from Barbour et al. 1996
  # 0 = The median of both ref and deg plots overlap the interquartile
  # range of the other category.
  # 1 = One median (either ref or deg) overlaps the interquartile range
  # of the other category.
  # 2 = The interquartiles overlap but neither median overlaps with the
  # interquartile range of the other category.
  # 3 = The interquartile ranges do not overlap.
  final.df <- dplyr::full_join(ref.df, deg.df,
                               by = c("metric")) %>%
    dplyr::group_by(metric) %>%
    dplyr::mutate(barbour_sensitivity = dplyr::case_when(
      ref_quant_50 <= deg_quant_75 & ref_quant_50 >= deg_quant_25 &
        ref_quant_75 >= deg_quant_50 & ref_quant_75 <= deg_quant_25 ~ 0,
      ref_quant_50 <= deg_quant_75 & ref_quant_50 >= deg_quant_25 |
        ref_quant_75 >= deg_quant_50 & ref_quant_75 <= deg_quant_25 ~ 1,

      ref_quant_25 <= deg_quant_75 & ref_quant_25 >= deg_quant_25 |
        ref_quant_75 <= deg_quant_75 & ref_quant_75 >= deg_quant_25 ~ 2,
      TRUE ~ 3
    )) %>%
    dplyr::select(metric, barbour_sensitivity) %>%
    dplyr::arrange(dplyr::desc(barbour_sensitivity))


  return(final.df)
}
