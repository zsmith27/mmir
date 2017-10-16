#==============================================================================
#'Discrimination Efficiency
#'
#'@param deg.df = a data frame of only the lower.class values.
#'@param quant.df = Data frame containing upper.class quantile values.
#'@return Determines the threshold at which a metric best categorizes
#'reference and degraded stations.
#'@export

original_de <- function(deg.df, quant.df, first.metric){
  
  metric_col.1 <- which(names(deg.df) %in% first.metric)
  last.metric <- names(deg.df)[ncol(deg.df)]
  
  if(is.null(names(metrics.df[, metric_col.1:ncol(metrics.df)]))){
    deg.long <- tidyr::gather_(deg.df, "METRICS", "VALUES",
                             noquote(names(metrics.df)[metric_col.1]))
  } else {
    deg.long <- tidyr::gather_(deg.df, "METRICS", "VALUES",
                             noquote(names(metrics.df[, metric_col.1:ncol(metrics.df)])))
  }
  #deg.long <- tidyr::gather_(deg.df, "METRICS", "VALUES",
  #                           noquote(names(deg.df[, metric_col.1:ncol(deg.df)])))

  merged <- merge(deg.long, quant.df, by = "METRICS")
  #============================================================================

  merged$SENSITIVITY <- ifelse(merged$DISTURBANCE %in% "DECREASE" &
                       merged$VALUES < merged$`25%`, 1,
                     ifelse(merged$DISTURBANCE %in% "DECREASE" &
                              merged$VALUES >= merged$`25%`, 0,
                            ifelse(merged$DISTURBANCE %in% "INCREASE" &
                                     merged$VALUES > merged$`75%`, 1,
                                   ifelse(merged$DISTURBANCE %in% "INCREASE" &
                                            merged$VALUES <= merged$`75%`, 0,
                                          ifelse(merged$DISTURBANCE %in% "EQUAL", 0, 1000)))))
  #============================================================================
  if(!is.na(sum(merged$SENSITIVITY))){
    sub.merged <- merged[, c("METRICS", "DISTURBANCE", "SENSITIVITY")]
    agg <- aggregate(SENSITIVITY ~ METRICS + DISTURBANCE, data = sub.merged, FUN = sum)
    agg$SENSITIVITY <- (agg$SENSITIVITY / nrow(deg.df)) * 100
    final.df <- agg
  }else{
    final.df <- data.frame(METRICS = names(deg.df[metric_col.1:ncol(deg.df)]))
    final.df$DISTURBANCE <- NA
    final.df$SENSITIVITY <- NA
  }
  
  return(final.df)
}
