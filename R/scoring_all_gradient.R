#==============================================================================
#'Scoring Method: All Site Classes Gradient (5th-95th)
#'
#'@param metrics.df = data frame of metric values for each station
#'@return Scores the samples.
#'@export

all_gradient_5_95 <- function(metrics.df, sensitivity.df, first.metric){
  metric_col.1 <- which(names(metrics.df) %in% first.metric)
  #============================================================================
  if (is.null(names(metrics.df[, metric_col.1:ncol(metrics.df)]))) {
    all.quant <- data.frame(t(quantile(metrics.df[, metric_col.1],
                                     c(0.05, 0.95))))
    all.quant$METRICS <- names(metrics.df)[metric_col.1]
  } else {
    all.quant <- data.frame(t(sapply(metrics.df[, metric_col.1:ncol(metrics.df)], quantile,
                                     c(0.05, 0.95), na.rm = TRUE)))
    all.quant$METRICS <- row.names(all.quant)
  }
  
  names(all.quant) <- c("ALL_5%", "ALL_95%", "METRICS")
  
  sensitivity.df <- sensitivity.df[, c("METRICS", "DISTURBANCE")]
  score.info <- merge(sensitivity.df, all.quant, by = "METRICS")
  #============================================================================
  if (is.null(names(metrics.df[, metric_col.1:ncol(metrics.df)]))) {
    metrics.cols <- names(metrics.df)[metric_col.1]
  } else {
    metrics.cols <- names(metrics.df[, metric_col.1:ncol(metrics.df)])
  }
  
  long.df <- tidyr::gather_(metrics.df, "METRICS", "REPORTING_VALUE",
                            noquote(metrics.cols))
  long.df <- merge(long.df, score.info, by = "METRICS")
  #============================================================================
  long.df$ALL_GRADIENT_5_95 <- ifelse(long.df$DISTURBANCE %in% c("DECREASE", "EQUAL") &
                                   long.df$REPORTING_VALUE <= long.df$`ALL_5%`, 0,
                                 ifelse(long.df$DISTURBANCE %in% c("DECREASE", "EQUAL") &
                                          long.df$REPORTING_VALUE > long.df$`ALL_5%` &
                                          long.df$REPORTING_VALUE < long.df$`ALL_95%`,
                                        ((long.df$REPORTING_VALUE - long.df$`ALL_5%`) /
                                           (long.df$`ALL_95%` - long.df$`ALL_5%`)) * 100,
                                        ifelse(long.df$DISTURBANCE %in% c("DECREASE", "EQUAL") &
                                                 long.df$REPORTING_VALUE >= long.df$`ALL_95%`, 100,
                                               ifelse(long.df$DISTURBANCE %in% "INCREASE" &
                                                        long.df$REPORTING_VALUE <= long.df$`ALL_5%`, 100,
                                                      ifelse(long.df$DISTURBANCE %in% "INCREASE" &
                                                               long.df$REPORTING_VALUE > long.df$`ALL_5%` &
                                                               long.df$REPORTING_VALUE < long.df$`ALL_95%` ,
                                                             ((long.df$`ALL_95%` - long.df$REPORTING_VALUE) /
                                                                (long.df$`ALL_95%` - long.df$`ALL_5%`)) * 100,
                                                             ifelse(long.df$DISTURBANCE %in% "INCREASE" &
                                                                      long.df$REPORTING_VALUE >= long.df$`ALL_95%`, 0, 100000))))))
  #============================================================================
  long.df$ALL_GRADIENT_5_95 <- round(long.df$ALL_GRADIENT_5_95, digits = 2)
  #============================================================================
  remove.cols <- c("REPORTING_VALUE", "ALL_5%", "ALL_95%", "DISTURBANCE")
  long.df <- long.df[, !names(long.df) %in% remove.cols]
  #============================================================================
  final.df <- tidyr::spread(long.df, METRICS, ALL_GRADIENT_5_95)
  #============================================================================
  site_info.cols <- names(metrics.df[, 1:(metric_col.1 - 1)])
  if (is.null(names(metrics.df[, metric_col.1:ncol(metrics.df)]))) {
    metrics.cols <- names(metrics.df)[metric_col.1]
  } else {
    metrics.cols <- names(metrics.df[, metric_col.1:ncol(metrics.df)])
  }
  final.df <- final.df[, c(site_info.cols, metrics.cols)]
  return(final.df)
}

#==============================================================================
#'Scoring Method: All Site Classes Gradient (Min - Max)
#'
#'@param metrics.df = data frame of metric values for each station
#'@return Scores the samples.
#'@export

all_gradient_min_max <- function(metrics.df, sensitivity.df, first.metric){
  metric_col.1 <- which(names(metrics.df) %in% first.metric)
  #============================================================================
  if (is.null(names(metrics.df[, metric_col.1:ncol(metrics.df)]))) {
    all.quant <- data.frame(t(quantile(metrics.df[, metric_col.1],
                                       c(0, 1))))
    all.quant$METRICS <- names(metrics.df)[metric_col.1]
  } else {
    all.quant <- data.frame(t(sapply(metrics.df[, metric_col.1:ncol(metrics.df)], quantile,
                                     c(0, 1), na.rm = TRUE)))
    all.quant$METRICS <- row.names(all.quant)
  }
  names(all.quant) <- c("ALL_MIN", "ALL_MAX", "METRICS")
  sensitivity.df <- sensitivity.df[, c("METRICS", "DISTURBANCE")]
  score.info <- merge(sensitivity.df, all.quant, by = "METRICS")
  #============================================================================
  if (is.null(names(metrics.df[, metric_col.1:ncol(metrics.df)]))) {
    metrics.cols <- names(metrics.df)[metric_col.1]
  } else {
    metrics.cols <- names(metrics.df[, metric_col.1:ncol(metrics.df)])
  }
  long.df <- tidyr::gather_(metrics.df, "METRICS", "REPORTING_VALUE",
                            noquote(metrics.cols))
  long.df <- merge(long.df, score.info, by = "METRICS")
  #============================================================================
  long.df$ALL_GRADIENT_MIN_MAX  <- ifelse(long.df$DISTURBANCE %in% c("DECREASE", "EQUAL") &
                                   long.df$REPORTING_VALUE <= long.df$ALL_MIN, 0,
                                 ifelse(long.df$DISTURBANCE %in% c("DECREASE", "EQUAL") &
                                          long.df$REPORTING_VALUE > long.df$ALL_MIN &
                                          long.df$REPORTING_VALUE < long.df$ALL_MAX ,
                                        ((long.df$REPORTING_VALUE - long.df$ALL_MIN) /
                                           (long.df$ALL_MAX - long.df$ALL_MIN)) * 100,
                                        ifelse(long.df$DISTURBANCE %in% c("DECREASE", "EQUAL") &
                                                 long.df$REPORTING_VALUE >= long.df$ALL_MAX, 100,
                                               ifelse(long.df$DISTURBANCE %in% "INCREASE" &
                                                        long.df$REPORTING_VALUE <= long.df$ALL_MIN, 100,
                                                      ifelse(long.df$DISTURBANCE %in% "INCREASE" &
                                                               long.df$REPORTING_VALUE > long.df$ALL_MIN &
                                                               long.df$REPORTING_VALUE < long.df$ALL_MAX ,
                                                             ((long.df$ALL_MAX - long.df$REPORTING_VALUE) /
                                                                (long.df$ALL_MAX - long.df$ALL_MIN)) * 100,
                                                             ifelse(long.df$DISTURBANCE %in% "INCREASE" &
                                                                      long.df$REPORTING_VALUE >= long.df$ALL_MAX, 0, 100000))))))
  #============================================================================
  long.df$ALL_GRADIENT_MIN_MAX <- round(long.df$ALL_GRADIENT_MIN_MAX , digits = 2)
  #============================================================================
  remove.cols <- c("REPORTING_VALUE", "ALL_MIN", "ALL_MAX", "DISTURBANCE")
  long.df <- long.df[, !names(long.df) %in% remove.cols]
  #============================================================================
  final.df <- tidyr::spread(long.df, METRICS, ALL_GRADIENT_MIN_MAX)
  #============================================================================
  site_info.cols <- names(metrics.df[, 1:(metric_col.1 - 1)])
  if (is.null(names(metrics.df[, metric_col.1:ncol(metrics.df)]))) {
    metrics.cols <- names(metrics.df)[metric_col.1]
  } else {
    metrics.cols <- names(metrics.df[, metric_col.1:ncol(metrics.df)])
  }
  final.df <- final.df[, c(site_info.cols, metrics.cols)]
  return(final.df)
}

#==============================================================================
#'Scoring Method: All Site Classes Gradient (Min - 95th)
#'
#'@param metrics.df = data frame of metric values for each station
#'@return Scores the samples.
#'@export

all_gradient_min_95 <- function(metrics.df, sensitivity.df, first.metric){
  metric_col.1 <- which(names(metrics.df) %in% first.metric)
  #============================================================================
  if (is.null(names(metrics.df[, metric_col.1:ncol(metrics.df)]))) {
    all.quant <- data.frame(t(quantile(metrics.df[, metric_col.1],
                                       c(0, 0.05, 0.95, 1))))
    all.quant$METRICS <- names(metrics.df)[metric_col.1]
  } else {
    all.quant <- data.frame(t(sapply(metrics.df[, metric_col.1:ncol(metrics.df)], quantile,
                                     c(0, 0.05, 0.95, 1))))
    all.quant$METRICS <- row.names(all.quant)
  }
  names(all.quant) <- c("ALL_MIN", "ALL_05", "ALL_95", "ALL_MAX", "METRICS")
  sensitivity.df <- sensitivity.df[, c("METRICS", "DISTURBANCE")]
  score.info <- merge(sensitivity.df, all.quant, by = "METRICS")
  #============================================================================
  if (is.null(names(metrics.df[, metric_col.1:ncol(metrics.df)]))) {
    metrics.cols <- names(metrics.df)[metric_col.1]
  } else {
    metrics.cols <- names(metrics.df[, metric_col.1:ncol(metrics.df)])
  }
  long.df <- tidyr::gather_(metrics.df, "METRICS", "REPORTING_VALUE",
                            noquote(metrics.cols))
  long.df <- merge(long.df, score.info, by = "METRICS")
  #============================================================================
  long.df$ALL_GRADIENT_MIN_MAX  <- ifelse(long.df$DISTURBANCE %in% c("DECREASE", "EQUAL") &
                                            long.df$REPORTING_VALUE <= long.df$ALL_MIN, 0,
                                          ifelse(long.df$DISTURBANCE %in% c("DECREASE", "EQUAL") &
                                                   long.df$REPORTING_VALUE > long.df$ALL_MIN &
                                                   long.df$REPORTING_VALUE < long.df$ALL_95 ,
                                                 ((long.df$REPORTING_VALUE - long.df$ALL_MIN) /
                                                    (long.df$ALL_95 - long.df$ALL_MIN)) * 100,
                                                 ifelse(long.df$DISTURBANCE %in% c("DECREASE", "EQUAL") &
                                                          long.df$REPORTING_VALUE >= long.df$ALL_95, 100,
                                                        ifelse(long.df$DISTURBANCE %in% "INCREASE" &
                                                                 long.df$REPORTING_VALUE <= long.df$ALL_05, 100,
                                                               ifelse(long.df$DISTURBANCE %in% "INCREASE" &
                                                                        long.df$REPORTING_VALUE > long.df$ALL_05 &
                                                                        long.df$REPORTING_VALUE < long.df$ALL_MAX ,
                                                                      ((long.df$ALL_MAX - long.df$REPORTING_VALUE) /
                                                                         (long.df$ALL_MAX - long.df$ALL_05)) * 100,
                                                                      ifelse(long.df$DISTURBANCE %in% "INCREASE" &
                                                                               long.df$REPORTING_VALUE >= long.df$ALL_MAX, 0, 100000))))))
  #============================================================================
  long.df$ALL_GRADIENT_MIN_MAX <- round(long.df$ALL_GRADIENT_MIN_MAX , digits = 2)
  #============================================================================
  remove.cols <- c("REPORTING_VALUE", "ALL_MIN", "ALL_05", "ALL_95",
                   "ALL_MAX", "DISTURBANCE")
  long.df <- long.df[, !names(long.df) %in% remove.cols]
  #============================================================================
  final.df <- tidyr::spread(long.df, METRICS, ALL_GRADIENT_MIN_MAX)
  #============================================================================
  site_info.cols <- names(metrics.df[, 1:(metric_col.1 - 1)])
  if (is.null(names(metrics.df[, metric_col.1:ncol(metrics.df)]))) {
    metrics.cols <- names(metrics.df)[metric_col.1]
  } else {
    metrics.cols <- names(metrics.df[, metric_col.1:ncol(metrics.df)])
  }
  final.df <- final.df[, c(site_info.cols, metrics.cols)]
  return(final.df)
}
#==============================================================================
#'Scoring Method: CAU (0-95th)
#'
#'@param metrics.df = data frame of metric values for each station
#'@return *** NOT COMPLETE ***. Scores the samples. 
#'@export

all_gradient_CAU <- function(metrics.df, sensitivity.df, first.metric){
  metric_col.1 <- which(names(metrics.df) %in% first.metric)
  #============================================================================
  if (is.null(names(metrics.df[, metric_col.1:ncol(metrics.df)]))) {
    all.quant <- data.frame(t(quantile(metrics.df[, metric_col.1],
                                       c(0.05, 0.95))))
    all.quant$METRICS <- names(metrics.df)[metric_col.1]
  } else {
    all.quant <- data.frame(t(sapply(metrics.df[, metric_col.1:ncol(metrics.df)], quantile,
                                     c(0.05, 0.95), na.rm = TRUE)))
    all.quant$METRICS <- row.names(all.quant)
  }
  
  names(all.quant) <- c("ALL_5%", "ALL_95%", "METRICS")
  
  sensitivity.df <- sensitivity.df[, c("METRICS", "DISTURBANCE")]
  score.info <- merge(sensitivity.df, all.quant, by = "METRICS")
  #============================================================================
  if (is.null(names(metrics.df[, metric_col.1:ncol(metrics.df)]))) {
    metrics.cols <- names(metrics.df)[metric_col.1]
  } else {
    metrics.cols <- names(metrics.df[, metric_col.1:ncol(metrics.df)])
  }
  
  long.df <- tidyr::gather_(metrics.df, "METRICS", "REPORTING_VALUE",
                            noquote(metrics.cols))
  long.df <- merge(long.df, score.info, by = "METRICS")
  #============================================================================
  long.df$ALL_GRADIENT_CAU <- ifelse(long.df$DISTURBANCE %in% c("DECREASE", "EQUAL") &
                                        long.df$REPORTING_VALUE <= 0, 0,
                                      ifelse(long.df$DISTURBANCE %in% c("DECREASE", "EQUAL") &
                                               long.df$REPORTING_VALUE > 0 &
                                               long.df$REPORTING_VALUE < long.df$`ALL_95%`,
                                             ((long.df$REPORTING_VALUE - 0) /
                                                (long.df$`ALL_95%` - 0)) * 100,
                                             ifelse(long.df$DISTURBANCE %in% c("DECREASE", "EQUAL") &
                                                      long.df$REPORTING_VALUE >= long.df$`ALL_95%`, 100,
                                                    ifelse(long.df$DISTURBANCE %in% "INCREASE" &
                                                             long.df$REPORTING_VALUE <= long.df$`ALL_5%`, 100,
                                                           ifelse(long.df$DISTURBANCE %in% "INCREASE" &
                                                                    long.df$REPORTING_VALUE > long.df$`ALL_5%` &
                                                                    long.df$REPORTING_VALUE < 100 ,
                                                                  ((100 - long.df$REPORTING_VALUE) /
                                                                     (100 - long.df$`ALL_5%`)) * 100,
                                                                  ifelse(long.df$DISTURBANCE %in% "INCREASE" &
                                                                           long.df$REPORTING_VALUE >= 100, 0, 100000))))))
  #============================================================================
  long.df$ALL_GRADIENT_CAU <- round(long.df$ALL_GRADIENT_CAU, digits = 2)
  #============================================================================
  remove.cols <- c("REPORTING_VALUE", "ALL_5%", "ALL_95%", "DISTURBANCE")
  long.df <- long.df[, !names(long.df) %in% remove.cols]
  #============================================================================
  final.df <- tidyr::spread(long.df, METRICS, ALL_GRADIENT_CAU)
  #============================================================================
  site_info.cols <- names(metrics.df[, 1:(metric_col.1 - 1)])
  if (is.null(names(metrics.df[, metric_col.1:ncol(metrics.df)]))) {
    metrics.cols <- names(metrics.df)[metric_col.1]
  } else {
    metrics.cols <- names(metrics.df[, metric_col.1:ncol(metrics.df)])
  }
  final.df <- final.df[, c(site_info.cols, metrics.cols)]
  return(final.df)
}

