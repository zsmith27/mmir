#' @title Sensitivity: Youden's Index
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
#'  \code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarize}},\code{\link[dplyr]{ungroup}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{case_when}},\code{\link[dplyr]{quo_name}},\code{\link[dplyr]{bind_rows}},\code{\link[dplyr]{left_join}},\code{\link[dplyr]{}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{desc}}

#'  \code{\link[OptimalCutpoints]{optimal.cutpoints}},\code{\link[OptimalCutpoints]{control.cutpoints}}

#'  \code{\link[rlang]{quos}},\code{\link[rlang]{enquo}},\code{\link[rlang]{sym}}

#'  \code{\link[tidyr]{gather}},\code{\link[tidyr]{spread}}
#' @rdname sensitivity_youden
#' @export
#' @importFrom dplyr group_by summarize ungroup mutate case_when quo_name bind_rows left_join  mutate select arrange desc
#' @importFrom OptimalCutpoints optimal.cutpoints control.cutpoints
#' @importFrom rlang quos enquo sym
#' @importFrom tidyr gather spread

sensitivity_youden <- function(metrics.df, condition.col, ref.cond, deg.cond, ...) {
  metric.cols <- rlang::quos(...)
  condition.col <- rlang::enquo(condition.col)
  ref.quo <- rlang::sym(ref.cond)
  deg.quo <- rlang::sym(deg.cond)
  #----------------------------------------------------------------------------
  metrics.vec <- metrics.df %>%
    select(!!!metric.cols) %>%
    names()
  #----------------------------------------------------------------------------
  metric.info <- metrics.df %>%
    tidyr::gather(metric, value, !!!metric.cols) %>%
    dplyr::group_by(!!condition.col, metric) %>%
    dplyr::summarize(median = median(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(!!condition.col, median) %>%
    dplyr::mutate(disturbance = dplyr::case_when(
      (!!ref.quo) > (!!deg.quo) ~ "decrease",
      (!!ref.quo) < (!!deg.quo) ~ "increase",
      TRUE ~ "equal"
    ))
  #----------------------------------------------------------------------------
  youden.df <- lapply(metrics.vec, function(metric.i) {
    info.sub <- metric.info %>%
      filter(metric == metric.i)
    dir.vec <- ifelse(info.sub$disturbance == "increase", "<", ">")
    youden.df <- OptimalCutpoints::optimal.cutpoints(X = metric.i,
                                                     status = dplyr::quo_name(condition.col),
                                                     tag.healthy = ref.cond,
                                                     methods = "Youden",
                                                     data = metrics.df,
                                                     direction = dir.vec,
                                                     control = OptimalCutpoints::control.cutpoints(),
                                                     ci.fit = FALSE,
                                                     conf.level = 0.95,
                                                     trace = FALSE)
    final.df <- data.frame(metric = metric.i,
                           youden_threshold = youden.df$Youden$Global$optimal.cutoff$cutoff,
                           disturbance = unique(info.sub$disturbance),
                           stringsAsFactors = FALSE)
  }) %>%
    dplyr::bind_rows()

  final.df <- metrics.df %>%
    tidyr::gather(metric, value, !!!metric.cols) %>%
    dplyr::left_join(youden.df, by = "metric") %>%
    dplyr::group_by(metric, youden_threshold, !!condition.col, disturbance) %>%
    dplyr::summarize(correct_class = sum(value >= youden_threshold) / n() * 100) %>%
    dplyr::ungroup() %>%
    tidyr::spread(!!condition.col, correct_class) %>%
    dplyr:: mutate(!!deg.quo := 100 - !!deg.quo,
                   youden_sensitivity = ((!!deg.quo) + (!!ref.quo)) / 2) %>%
    dplyr::select(-(!!ref.quo), -(!!deg.quo)) %>%
    dplyr::arrange(dplyr::desc(youden_sensitivity))

  return(final.df)

}
