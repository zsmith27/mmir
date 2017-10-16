#==============================================================================
#Metric Sensitivity
#==============================================================================
#'Metric Sensitivity
#'
#'@param metrics.df = data frame of metric values for each station
#'@param ref.cond = The site classification that represents better
#'environmental conditions.
#'@param deg.cond = The site classification that represents the degraded
#'environmental conditions.
#'@param method = the sensitivity function to be used during the assessment.
#'@return Determines the threshold at which a metric best categorizes
#'reference and degraded stations.
#'@export
#'
sensitivity <- function(metrics.df, first.metric, condition.colname,
                        ref.cond = "REF", deg.cond = "DEG",
                        method = "BDE"){
 
  #Create new data frames specific for Degraded and Reference sites
  deg.df <- metrics.df[metrics.df[, condition.colname] %in% deg.cond, ]
  ref.df <- metrics.df[metrics.df[, condition.colname] %in% ref.cond, ]
  #============================================================================
  #Calculate the median values for the reference and degraded distributions.
  metric_col.1 <- which(names(metrics.df) %in% first.metric)
  if(ncol(metrics.df) == metric_col.1){
    ref_50 <- quantile(ref.df[, metric_col.1], 0.50, na.rm = TRUE)
    deg_50 <- quantile(deg.df[, metric_col.1], 0.50, na.rm = TRUE)
    
    #Provide the each reference percentile value for each metric.
    quant.ref <- data.frame(quantile(ref.df[, metric_col.1], probs = seq(0, 1, by = 0.01), na.rm = TRUE))
    colnames(quant.ref) <- colnames(ref.df)[metric_col.1]
    #Create a column listing all of the metrics and join the reference percentile values
    quant.df <- cbind(data.frame(colnames(metrics.df[metric_col.1])), t(quant.ref))
    names(quant.df)[1] <- "METRICS" #Rename column 1
    
  
    #Create new data frames specific for Degraded and Reference sites
    severe.df <- metrics.df[metrics.df[, condition.colname] %in% deg.cond, ]
    reference.df <- metrics.df[metrics.df[, condition.colname] %in% ref.cond, ]
    #Calculate the median values for the reference and degraded distributions.
    reference_50 <- quantile(reference.df[, metric_col.1],  0.50, na.rm = TRUE)
    severe_50 <- quantile(severe.df[, metric_col.1],  0.50, na.rm = TRUE)
    #Insert a column to suggest how the metric reacts to disturbance. If the reference median
    # is greater than the degraded median, the metric decreases with distrubance. If the reference
    # median is less than the degraded median, the metric increases with disturbance.  If the
    # medians are equal, equal is return to indicate that this metric shows no distinction between
    # reference and degraded contions.
    quant.df$DISTURBANCE <- ifelse(reference_50 > severe_50, "DECREASE",
                                   ifelse(reference_50 < severe_50, "INCREASE", "EQUAL"))
    
  }
  
  if(length(ref.df) > metric_col.1){
    ref_50 <- sapply(ref.df[, metric_col.1:ncol(ref.df)], quantile, 0.50, na.rm = TRUE)
    deg_50 <- sapply(deg.df[, metric_col.1:ncol(deg.df)], quantile, 0.50, na.rm = TRUE)
    
    #Provide the each reference percentile value for each metric.
    quant.ref <- data.frame(apply(ref.df[, metric_col.1:ncol(ref.df)], 2, function(x) quantile(x, probs = seq(0, 1, by = 0.01), na.rm = TRUE)))
    #Create a column listing all of the metrics and join the reference percentile values
    quant.df <- cbind(data.frame(colnames(metrics.df[metric_col.1:ncol(metrics.df)])), t(quant.ref))
    names(quant.df)[1] <- "METRICS" #Rename column 1

    
    #Create new data frames specific for Degraded and Reference sites
    severe.df <- metrics.df[metrics.df[, condition.colname] %in% deg.cond, ]
    reference.df <- metrics.df[metrics.df[, condition.colname] %in% ref.cond, ]
    #Calculate the median values for the reference and degraded distributions.
    reference_50 <- sapply(reference.df[, metric_col.1:ncol(reference.df)], quantile, 0.50, na.rm = TRUE)
    severe_50 <- sapply(severe.df[, metric_col.1:ncol(severe.df)], quantile, 0.50, na.rm = TRUE)
    #Insert a column to suggest how the metric reacts to disturbance. If the reference median
    # is greater than the degraded median, the metric decreases with distrubance. If the reference
    # median is less than the degraded median, the metric increases with disturbance.  If the
    # medians are equal, equal is return to indicate that this metric shows no distinction between
    # reference and degraded contions.
    quant.df$DISTURBANCE <- ifelse(reference_50 > severe_50, "DECREASE",
                                   ifelse(reference_50 < severe_50, "INCREASE", "EQUAL"))
  }
  
  #============================================================================
  if(method %in% c("BARBOUR", "ALL")){
    barb.df <- barbour(metrics.df, first.metric, ref.df, deg.df)
  }
  
  if(method %in% c("DE", "ALL")){
    de.df <- original_de(deg.df, quant.df, first.metric)
  }
  
  if(method %in% c("BDE", "ALL")){
    bde.df <- balanced_ce(metrics.df, first.metric, quant.df,
                            condition.colname,
                            ref.cond, deg.cond,
                            ref.df, deg.df, quant.ref)
  }
  
  if(method %in% "ALL"){
    names(barb.df)[names(barb.df) %in% "SENSITIVITY"] <- "BARBOUR_SENSITIVITY"
    names(de.df)[names(de.df) %in% "SENSITIVITY"] <- "DE_SENSITIVITY"
    names(bde.df)[names(bde.df) %in% "SENSITIVITY"] <- "BDE_SENSITIVITY"
    final.df <- plyr::join_all(list(barb.df, de.df, bde.df), c("METRICS", "DISTURBANCE"))
  } else {
    if(exists("barb.df")) final.df <- barb.df
    if(exists("de.df")) final.df <- de.df
    if(exists("bde.df")) final.df <- bde.df
  }
  
  
  return(final.df)
}