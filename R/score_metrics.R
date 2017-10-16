#==============================================================================
# Score Metrics
#==============================================================================
#'Score Metrics
#'
#'@param metrics.df <- a data frame of samples to be scored.
#'@return A wrapper function that allows you to select your scoring methodology
#'and a metric sensitivity threshold.
#'@export


score_metrics <- function(metrics.df, scoring.method,
                          sensitivity.df, sensitivity.colname, sensitivity.threshold = 0,
                          first.metric, condition.colname = NULL, ref.cond = NULL) {

  #============================================================================
  if (sensitivity.threshold > 0) {
    test.sens <- sensitivity.df[sensitivity.df[, sensitivity.colname] >= sensitivity.threshold, ]
    if (nrow(test.sens) == 0){
      error.mess <- paste0("No metrics greater than or equal to the given ",
                          "sensitivity threshold (sensitivity.threshold = ",
                          sensitivity.threshold, ").")
      stop(error.mess)
    } else {
      sensitivity.df <- test.sens
    }
    sensitivity.df <- sensitivity.df[order(sensitivity.df[, sensitivity.colname],
                                           decreasing = TRUE), ]
    metric_col.1 <- which(names(metrics.df) %in% first.metric)
    site_id.cols <- names(metrics.df[, 1:(metric_col.1 - 1)])
    metric.cols <- unique(sensitivity.df$METRICS)
    metrics.df <- metrics.df[, c(site_id.cols, metric.cols)]
    first.metric <- metric.cols[1]
  }
  #============================================================================
  if(scoring.method %in% "balanced_even") {
    final.df <- balanced_scoring(metrics.df, sensitivity.df,
                                    first.metric,
                                    method = "EVEN")
  }
  #============================================================================
  if(scoring.method %in% "balanced_uneven") {
    final.df <- balanced_scoring(metrics.df, sensitivity.df,
                                  first.metric,
                                  method = "UNEVEN")
  }
  #============================================================================
  if(scoring.method %in% "all_gradient_min_max") {
    final.df <- all_gradient_min_max(metrics.df, sensitivity.df, first.metric)
  }
  #============================================================================
  if(scoring.method %in% "all_gradient_min_95") {
    final.df <- all_gradient_min_95(metrics.df, sensitivity.df, first.metric)
  }
  #============================================================================
  if(scoring.method %in% "all_gradient_5_95") {
    final.df <- all_gradient_5_95(metrics.df, sensitivity.df, first.metric)
  }
  #============================================================================
  
  if(scoring.method %in% "CAU") {
    final.df <- all_gradient_CAU(metrics.df, sensitivity.df, first.metric)
  }
  #============================================================================
  if(scoring.method %in% "ref_gradient") {
    final.df <- ref_gradient(metrics.df, sensitivity.df,
                                 first.metric,
                                 condition.colname,
                                 ref.cond)
  }
  #============================================================================
  if(scoring.method %in% "ref_categorical") {
    final.df <- ref_categorical(metrics.df, sensitivity.df,
                              first.metric,
                              condition.colname,
                              ref.cond)
  }
  #============================================================================
  return(final.df)
}