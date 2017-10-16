#==============================================================================
# Balanced Classification Efficiency
#==============================================================================
#'Balanced Classification Efficiency
#'@param metrics.df = data frame of metric values for each station with site
#'a column of site classes defined by environmental variables.
#'@param quant.df = Data frame containing ref.cond quantile values.
#'@param ref.cond = the site class that represents the better condition.
#'@param deg.cond = the site class that represents the poorer condition.
#'@param ref.df = a data frame of only the ref.cond values.
#'@param quant.ref = a data frame of reference quantile values.
#'@return Determines the threshold at which a metric best categorizes
#'reference and degraded stations.
#'@export
#'

balanced_ce <- function(metrics.df, first.metric, quant.df, condition.colname,
                        ref.cond, deg.cond, ref.df, deg.df, quant.ref){
  #Transform the metrics data frame from a wide format to a long data format.
  metric_col.1 <- which(names(metrics.df) %in% first.metric)
  if(is.null(names(metrics.df[, metric_col.1:ncol(metrics.df)]))){
    melted <- tidyr::gather_(metrics.df, "METRICS", "VALUES",
                             noquote(names(metrics.df)[metric_col.1]))
  } else {
    melted <- tidyr::gather_(metrics.df, "METRICS", "VALUES",
                             noquote(names(metrics.df[, metric_col.1:ncol(metrics.df)])))
  }
  #============================================================================
  #Merge the new long format metrics data frame with the quantile (percentile) values.
  long.df <- merge(melted, quant.df, by = "METRICS")
  #============================================================================
  #Create a new data frame of just reference values
  long.ref <- long.df[long.df[, condition.colname] %in% ref.cond, ]
  #Column numbers can change easily. The script below specifies column "0%" to column "100%."
  # These columns represent the percentile values.
  ref.columns <-  which(colnames(long.ref) == "0%") : which(colnames(long.ref) == "100%")
  #============================================================================
  # Looking for the percentage of sites correctly identified as reference or degraded based
  # on each threshold.  The thresholds are defined by the reference percentiles. If a site
  # is correctly identified as reference, then a 1 is returned. If the site is incorrectly
  # identified as degraded, then a 0 is returned.  Essentially, 1 is equivalent to "yes"
  # and 0 is equivalent to "no."  This ifelse statement is specific to reference sites.
  # Below the ifelse statement is specific to degraded sites. If the metric decreases
  # with disturbance and the raw metric score for a sampling event
  # is greater than the percentile value, then the site was correctly identified as
  # a reference site and a 1 is returned.
  long.ref[, ref.columns] <- ifelse((long.ref$VALUE >= long.ref[, ref.columns] &
                                       long.ref[, condition.colname] %in% ref.cond &
                                       long.ref$DISTURBANCE%in%  "DECREASE") |
                                      (long.ref$VALUE <= long.ref[, ref.columns] &
                                         long.ref[, condition.colname] %in% ref.cond &
                                         long.ref$DISTURBANCE %in% "INCREASE"), 1, 0)
  #============================================================================
  #Transform the long reference data frame to a wide format.
  ref.columns <-  which(colnames(long.ref) %in% "0%") : which(colnames(long.ref) %in% "100%")
  melted.ref <- tidyr::gather_(long.ref, "PERCENTILE", "PERCENTILE_COUNT",
                               noquote(names(long.ref[, ref.columns])))
  melted.ref <- melted.ref[, c("METRICS", "DISTURBANCE",
                               "PERCENTILE", "PERCENTILE_COUNT")]
  #============================================================================
  #Aggregate the values (1's and 0's) by distrubance, metric, and variable (percentile).
  # The aggregation function finds the mean of the values (1's and 0's) and multiplies
  # the mean by 100. This value represents the percentage of reference sites correctly
  # identified as reference sites for a particular metric at a particular
  # threshold (percentile).
  pct.ref <- aggregate(PERCENTILE_COUNT ~ METRICS + DISTURBANCE +  PERCENTILE,
                       data = melted.ref, function(x) mean(x) * 100)
  colnames(pct.ref) <- c("METRICS", "DISTURBANCE","PERCENTILE", "PCT_REF")
  #============================================================================
  #Create a new data frame of just degraded values.
  long.deg <- long.df[long.df[, condition.colname] %in% deg.cond, ]
  #Column numbers can change easily. The script below specifies column "0%" to column "100%."
  # These columns represent the percentile values.
  deg.columns <-  which(colnames(long.deg) %in% "0%") : which(colnames(long.deg) %in% "100%")
  #============================================================================
  #See above for further description. Same process for identifing the number
  # of reference sites correctly identified at each threshold but the script
  # below is for correctly identified degraded sites.
  long.deg[, deg.columns] <- ifelse((long.deg$VALUE < long.deg[, deg.columns] &
                                       long.deg$DISTURBANCE %in% "DECREASE") |
                                      (long.deg$VALUE > long.deg[, deg.columns] &
                                         long.deg$DISTURBANCE %in% "INCREASE"), 1, 0)
  #============================================================================
  #Transform the long degraded data frame to a wide format.
  melted.deg <- tidyr::gather_(long.deg, "PERCENTILE", "PERCENTILE_COUNT",
                               noquote(names(long.deg[, deg.columns])))
  melted.deg <- melted.deg[, c("METRICS", "DISTURBANCE",
                               "PERCENTILE", "PERCENTILE_COUNT")]
  #============================================================================
  #Aggregate the values (1's and 0's) by distrubance, metric, and variable (percentile).
  # The aggregation function finds the mean of the values (1's and 0's) and multiplies
  # the mean by 100. This value represents the percentage of degraded sites correctly
  # identified as degraded sites for a particular metric at a particular
  # threshold (percentile).
  pct.deg <- aggregate(PERCENTILE_COUNT ~ METRICS + DISTURBANCE +  PERCENTILE,
                       data = melted.deg, function(x) mean(x) * 100)
  colnames(pct.deg) <- c("METRICS","DISTURBANCE", "PERCENTILE", "PCT_REF")
  #dt <- data.table::data.table(melted.deg)
  #pct.deg <- dt[, mean(value) * 100, by = list(DISTURBANCE, METRICS, variable)]
  colnames(pct.deg) <- c("METRICS", "DISTURBANCE", "PERCENTILE", "PCT_DEG")
  #============================================================================
  #Merge the two tables containing the percent of reference and the percent of
  # degraded correctly identified.
  merge.pct <- merge(pct.ref, pct.deg, by = c("DISTURBANCE", "METRICS", "PERCENTILE"))
  #Calculate the discrimination efficiency of each threshold.
  # DE = (Correctly identified Reference + Correctly identified Degraded) / 2
  merge.pct$SENSITIVITY <- (merge.pct$PCT_REF + merge.pct$PCT_DEG) / 2
  
  #==========================================================================
  #cols.keep <- 1:which(names(long.df) %in% "VALUES") - 1
  #tp <- (merge.pct$PCT_REF * nrow(unique(long.df[long.df[, condition.colname] %in% ref.cond, cols.keep]))) / 100
  #fn <- nrow(unique(long.df[long.df[, condition.colname]%in% ref.cond, cols.keep])) - tp
  #tn <- (merge.pct$PCT_DEG * nrow(unique(long.df[long.df[, condition.colname] %in% deg.cond, cols.keep]))) / 100
  #fp <- nrow(unique(long.df[long.df[, condition.colname] %in% deg.cond, cols.keep])) - tn
  
  #merge.pct$ACCURACY <- ((tp + tn) / (tp + tn + fp + fn)) * 100
  #merge.pct$TPR <- tp / (tp + fn)
  #merge.pct$FPR <- fp / (fp + tn)
  #merge.pct$FNR <- fn / (tp + fn)
  classification_efficiency <- (merge.pct$PCT_REF + merge.pct$PCT_DEG) / 2
  balancing <- abs(merge.pct$PCT_REF - merge.pct$PCT_DEG)

  merge.pct$BAL_SENSITIVITY <- classification_efficiency - balancing
  
  
  #============================================================================
  #Aggregate the table to select the best DE score.  That is the max DE score for
  # each metric.
  agg.df <- aggregate(BAL_SENSITIVITY ~ METRICS, data= merge.pct, FUN = max)

  #============================================================================
  # Merge the aggregated data frame to the merged.pct data frame.  Represents the
  # percentile with the best DE score. However, several percentiles may have the same
  # DE score. Hence, "almost_best."
  almost_best.df <- unique(merge(agg.df, merge.pct, by = c("METRICS", "BAL_SENSITIVITY")))
  #============================================================================
  #Create a table to count the number of metrics with multiple thresholds.
  metrics.table <- data.frame(table(almost_best.df$METRICS))
  colnames(metrics.table) <- c("Metric", "Count")
  #============================================================================
  #A data frame containing only metrics with multiple thresholds.
  metric.repeats <- metrics.table[metrics.table$Count > 1, ]
  #============================================================================
  #A data frame containing only metrics with a single threshold.
  fine <- almost_best.df[!almost_best.df$METRICS %in% metric.repeats$Metric, ]
  fine$PERCENTILE <- as.numeric(gsub("%", "", fine$PERCENTILE))
  #============================================================================
  #Use the list of metrics from metric.repeats to further inspect
  # thresholds in almost_best.df.
  mult.metrics <- almost_best.df[almost_best.df$METRICS %in% metric.repeats$Metric, ]
  #============================================================================
  #The balance issue occurs infrequently but must be accounted for.
  # Sometimes the same DE score can be attained from different pct_ref and pct_deg values.
  # For example, 90% pct_ref and 10% pct_deg is equal to 50% pct_ref and 50% pct_deg.
  # In these cases we prefer the more balanced solution (i.e., 50% and 50%) because both
  # groups are better represented.  If the DE score is a product of 90% and 10%, then the
  # majority of the data is being binned into a single group.  Therefore, there is poor
  # distinction between reference and degraded conditions.
  # Balance = | pct_ref - pct_deg |
  # The smaller the value the better the balance between the two conditions.
  mult.metrics$BALANCE <- abs(mult.metrics$PCT_REF - mult.metrics$PCT_DEG)
  balanced.df <- plyr::ddply(mult.metrics, plyr::.(METRICS),
                             function(x) x[which(x$BALANCE == min(x$BALANCE)), ])
  balanced.df <- balanced.df[ , -which(names(balanced.df) %in% c("BALANCE"))]
  balanced.df$PERCENTILE <- as.numeric(gsub("%", "", balanced.df$PERCENTILE))
  #============================================================================
  #Create new data frames for each disturbance category. Disturbance indicates
  # how the metric responds to disturbance.
  dec.df <- balanced.df[balanced.df$DISTURBANCE %in% "DECREASE", ]
  inc.df <- balanced.df[balanced.df$DISTURBANCE %in% "INCREASE", ]
  equ.df <- balanced.df[balanced.df$DISTURBANCE %in% "EQUAL", ]
  #============================================================================
  #Most of the time the balance check does not uliminate multiple thresholds.
  # This final step selects the single best threshold for each metric.
  # If the metric decreases with disturbance, the lowest percentile is
  # selected as the threshold.  If the metric increases with disturbance
  # the largest percentile is selected as the threshold. If the metric
  # cannot distinquish between reference and degraded then no threshold
  # is returned and all values equal zero.
  final.dec <- plyr::ddply(dec.df, plyr::.(METRICS), function(x) x[which.min(x$PERCENTILE), ])
  final.inc <- plyr::ddply(inc.df, plyr::.(METRICS), function(x) x[which.max(x$PERCENTILE), ])
  final.equ <- plyr::ddply(equ.df, plyr::.(METRICS), function(x) x[which.max(x$PERCENTILE), ])
  #Join all of the final threshold values together.
  bound.df <- rbind(fine, final.dec, final.inc, final.equ)
  #============================================================================
  #Use the ref.df data frame created in the beginning of the function
  # to report the reference median value.
  if(is.null(names(metrics.df[, metric_col.1:ncol(metrics.df)]))){
    ref_quant.df <- data.frame(t(quantile(ref.df[, metric_col.1], c(0, 0.05, 0.50, 0.95, 1))))
    ref_quant.df$METRICS <- names(ref.df)[metric_col.1]
  } else {
    ref_quant.df <- data.frame(t(sapply(ref.df[, metric_col.1:ncol(ref.df)], quantile, c(0, 0.05, 0.50, 0.95, 1))))
    ref_quant.df$METRICS <- row.names(ref_quant.df)
  }
  #ref_quant.df <- data.frame(t(sapply(ref.df[, metric_col.1:ncol(ref.df)], quantile, c(0, 0.05, 0.50, 0.95, 1))))
  names(ref_quant.df) <- c("REF_MIN", "REF_05", "REF_MEDIAN", "REF_95", "REF_MAX", "METRICS")
  
  #============================================================================
  #Use the ref.df data frame created in the beginning of the function
  # to report the reference median value.
  if(is.null(names(metrics.df[, metric_col.1:ncol(metrics.df)]))){
    deg_quant.df <- data.frame(t(quantile(deg.df[, metric_col.1], c(0, 0.05, 0.50, 0.95, 1))))
    deg_quant.df$METRICS <- names(deg.df)[metric_col.1]
  } else {
    deg_quant.df <- data.frame(t(sapply(deg.df[, metric_col.1:ncol(deg.df)], quantile, c(0, 0.05, 0.50, 0.95, 1), na.rm = TRUE)))
    deg_quant.df$METRICS <- row.names(deg_quant.df)
  }
  #deg_quant.df <- data.frame(t(sapply(deg.df[, metric_col.1:ncol(deg.df)], quantile, c(0, 0.05, 0.50, 0.95, 1))))
  names(deg_quant.df) <- c("DEG_MIN", "DEG_05", "DEG_MEDIAN", "DEG_95", "DEG_MAX", "METRICS")
  
  #Join the reference median values for each metric to the bound
  # data frame with the SENSITIVITYs and percentile thresholds
  # for each metric. af = almost final
  #af.df <- merge(bound.df, med.df, by = "METRICS")
  af.df <- plyr::join_all(list(bound.df, ref_quant.df, deg_quant.df), by = "METRICS")
  af.df$PERCENTILE <- paste0(af.df$PERCENTILE, "%")
  #============================================================================
  #Use the quant.ref data frame created in the beginning of the function
  # to represent the actual threshold value that the chosen percentile
  # represents.
  #Create a new column from row names.
  quant.ref$PERCENTILE <- row.names(quant.ref)
  #============================================================================
  #Transform the data frame from a wide to long format.
  melted.pct <- tidyr::gather(quant.ref, METRICS, BSP, -PERCENTILE)
  names(melted.pct) <- c("PERCENTILE", "METRICS", "BSP")#change column names
  #Merge the threshold values with the almost final data frame (af.df).
  final.df <- merge(af.df, melted.pct, by = c("METRICS", "PERCENTILE"), all.x = TRUE)
  #Round the values to the hundreths place.
  round.list <- vapply(final.df, is.numeric, FUN.VALUE = logical(1))
  final.df[, round.list] <- round(final.df[, round.list], digits = 2)
  final.df <- final.df[, !names(final.df) %in% "BAL_SENSITIVITY"]
  #============================================================================
  final.df$BOUND <- ifelse(final.df$DISTURBANCE %in% c("DECREASE", "EQUAL"),
                           final.df$BSP - abs(final.df$REF_MEDIAN - final.df$BSP),
                           ifelse(final.df$DISTURBANCE %in% "INCREASE",
                                  final.df$BSP + abs(final.df$REF_MEDIAN - final.df$BSP),
                                  "ERROR"))
  #============================================================================
  reorder.cols <- c("METRICS", "SENSITIVITY", names(final.df)[!names(final.df) %in% c("METRICS", "SENSITIVITY")])
  final.df <- final.df[, reorder.cols]
  return(final.df)
}
