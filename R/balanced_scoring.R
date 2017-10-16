#==============================================================================
# Balanced Scoring
#==============================================================================
#'Balanced Scoring
#'@param metrics.df = data frame of metric values for each station with site
#'a column of site classes defined by environmental variables.
#'@param sensitivity.df = the data frame output from the metric_sensitivity 
#'function where method was set to "BDE" or "ALL."
#'@param first.metric = the first metric column that appears in your wide
#'data frame when reading from left to right. It is assumed that all columns
#'to the right of the first metric column are metrics. Any other columns will
#'result in an error or, if the column is numeric, it will be treated as if
#'it were a metric.
#'@param method = two method options exist. The first option is "UNEVEN", which
#'uses the reference and degraded distributions 5th and 95th percentiles to
#'define scoring thresholds.  "EVEN" uses the median of the reference 
#'distribution and...
#'@return Balanced scoring...
#'@export
#'

balanced_scoring <- function(metrics.df, sensitivity.df, first.metric, method){
  
  if (method %in% "UNEVEN") {
    sensitivity.df$CEILING <- ifelse(sensitivity.df$DISTURBANCE %in% c("DECREASE", "EQUAL"),
                                   sensitivity.df$REF_95,
                                   ifelse(sensitivity.df$DISTURBANCE %in% "INCREASE",
                                          sensitivity.df$REF_05, "ERROR"))
    sensitivity.df$FLOOR <- ifelse(sensitivity.df$DISTURBANCE %in% c("DECREASE", "EQUAL"),
                                   sensitivity.df$DEG_05,
                                   ifelse(sensitivity.df$DISTURBANCE %in% "INCREASE",
                                          sensitivity.df$REF_95, "ERROR"))
  } else {
    if (method %in% "EVEN") {
      sensitivity.df$CEILING <- ifelse(sensitivity.df$DISTURBANCE %in% c("DECREASE", "EQUAL"),
                                       sensitivity.df$REF_MEDIAN,
                                       ifelse(sensitivity.df$DISTURBANCE %in% "INCREASE",
                                              sensitivity.df$BOUND, "ERROR"))
      sensitivity.df$FLOOR <- ifelse(sensitivity.df$DISTURBANCE %in% c("DECREASE", "EQUAL"),
                                     sensitivity.df$BOUND,
                                     ifelse(sensitivity.df$DISTURBANCE %in% "INCREASE",
                                            sensitivity.df$REF_MEDIAN, "ERROR"))
    } else {
      sensitivity.df$CEILING <- "ERROR"
      sensitivity.df$FLOOR <- "ERROR"
    }
  }
  #============================================================================
  sub.sens <- sensitivity.df[, c("METRICS", "DISTURBANCE", "CEILING", "BSP", "FLOOR")]
  sub.sens[, 3:5] <- lapply(sub.sens[, 3:5], function(x) as.numeric(as.character(x)))
  #============================================================================
  #if(bound.limits == TRUE){
  #  sensitivity.df$CEILING <- ifelse(sensitivity.df$CEILING < 0, 0, 
  #                                   ifelse(sensitivity.df$CEILING > 100, 100, sensitivity.df$CEILING))
  #  sensitivity.df$FLOOR <- ifelse(sensitivity.df$FLOOR < 0, 0, 
  #                                   ifelse(sensitivity.df$FLOOR > 100, 100, sensitivity.df$FLOOR))
  #}
  #============================================================================
  metric_col.1 <- which(names(metrics.df) %in% first.metric)
  if (is.null(names(metrics.df[, metric_col.1:ncol(metrics.df)]))) {
    metrics.cols <- names(metrics.df)[metric_col.1]
  } else {
    metrics.cols <- names(metrics.df[, metric_col.1:ncol(metrics.df)])
  }

  long.df <- tidyr::gather_(metrics.df, "METRICS", "VALUE",
                           noquote(metrics.cols))
  
  merged.df <- merge(long.df, sub.sens, by = "METRICS")
  #============================================================================
  merged.df$SCORE <- bal_score_methods(merged.df, method)
  
  #============================================================================
  remove.cols <- c("VALUE", "CEILING", "BSP", "FLOOR", "DISTURBANCE")
  merged.df <- merged.df[, !names(merged.df) %in% remove.cols]
  #merged.df$SCORE <- as.numeric(as.character(merged.df$SCORE)) * 100
  #============================================================================
  final.df <- tidyr::spread(merged.df, METRICS, SCORE)
  #============================================================================
  site_info.cols <- names(final.df[, c(1:(metric_col.1 - 1))])
  final.df <- final.df[, c(site_info.cols, metrics.cols)]
  #============================================================================
  # Round all the metrics to the second decimal place.
  if (length(metrics.cols) > 1) {
    final.df[, metrics.cols] <- lapply(final.df[, metrics.cols],
                                       function(x) round(as.numeric(x), 2))
  } else {
    final.df[, metrics.cols] <- round(as.numeric(final.df[, metrics.cols]), 2)
  }
  
  return(final.df)
}

#==============================================================================
#'balanced score methods
#'@param merged.df = 
#'@param method = two method options exist. The first option is "UNEVEN", which
#'uses the reference and degraded distributions 5th and 95th percentiles to
#'define scoring thresholds.  "EVEN" uses the median of the reference 
#'distribution and...
#'@return Balanced scoring...
#'@export
#'

bal_score_methods <- function(merged.df, method){
  dec_func <- function(upper, lower, value) (value - lower) / (upper - lower)

  inc_func <- function(upper, lower, value) (upper - value) / (upper - lower)
    
  if (method %in% "EVEN") {
    dec.case <- ifelse(merged.df$VALUE <= merged.df$FLOOR, 0,
                         ifelse(merged.df$VALUE >= merged.df$CEILING, 100,
                                ifelse(merged.df$VALUE < merged.df$CEILING &
                                         merged.df$VALUE >= merged.df$FLOOR,
                                       dec_func(merged.df$CEILING, merged.df$FLOOR, merged.df$VALUE) * 100,
                                       "ERROR")))
    
    inc.case <- ifelse(merged.df$VALUE >= merged.df$CEILING, 0,
                         ifelse(merged.df$VALUE <= merged.df$FLOOR, 100,
                                ifelse(merged.df$VALUE > merged.df$FLOOR &
                                         merged.df$VALUE <= merged.df$CEILING,
                                       inc_func(merged.df$CEILING, merged.df$FLOOR, merged.df$VALUE) * 100, "ERROR")))
  } else {
    if (method %in% "UNEVEN") {
    dec.case <- ifelse(merged.df$VALUE <= merged.df$FLOOR, 0,
                         ifelse(merged.df$VALUE >= merged.df$CEILING, 100,
                                ifelse(merged.df$VALUE == merged.df$BSP, 50,
                                       ifelse(merged.df$VALUE > merged.df$BSP &
                                                merged.df$VALUE < merged.df$CEILING,
                                              dec_func(merged.df$CEILING, merged.df$BSP, merged.df$VALUE) * 50 + 50,
                                              ifelse(merged.df$VALUE > merged.df$FLOOR &
                                                       merged.df$VALUE < merged.df$BSP,
                                                     dec_func(merged.df$BSP, merged.df$FLOOR, merged.df$VALUE) * 50,
                                                     "ERROR")))))
    
    inc.case <- ifelse(merged.df$VALUE >= merged.df$FLOOR, 0,
                         ifelse(merged.df$VALUE <= merged.df$CEILING, 100,
                                ifelse(merged.df$VALUE == merged.df$BSP, 50,
                                       ifelse(merged.df$VALUE > merged.df$BSP &
                                                merged.df$VALUE < merged.df$FLOOR,
                                              inc_func(merged.df$FLOOR, merged.df$BSP, merged.df$VALUE) * 50,
                                              ifelse(merged.df$VALUE > merged.df$CEILING &
                                                       merged.df$VALUE < merged.df$BSP,
                                                     inc_func(merged.df$BSP, merged.df$CEILING, merged.df$VALUE) * 50 + 50,
                                                     "ERROR")))))
    }
  }
  
  final.vec <- ifelse(merged.df$DISTURBANCE %in% c("EQUAL", "DECREASE"), dec.case,
                            ifelse(merged.df$DISTURBANCE %in% "INCREASE", inc.case,
                                   "ERROR"))
  return(final.vec)
  
}

