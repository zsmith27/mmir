#==============================================================================
# Barbour et al. 1996 methodology
#==============================================================================

#'Barbour et al. 1996 method for testing metric sensitivity
#'
#'@param metrics.df = data frame of metric values for each station
#'@param ref.df = a data frame of only the upper.class values.
#'@param deg.df = a data frame of only the lower.class values.
#'@return A data frame of metrics scored using the Barbour et al. 1996 method
#'for testing metric sensitivity
#'@export
barbour <- function(metrics.df, first.metric, ref.df, deg.df){
  metric_col.1 <- which(names(metrics.df) %in% first.metric)
  barbour <- data.frame(colnames(metrics.df[metric_col.1:ncol(metrics.df)]))
  colnames(barbour) <- "METRICS" #rename column #1 "Metrics"
  #============================================================================
  #Quartiles for ref.df and DEG Categorys
  if(ncol(ref.df) > metric_col.1){
    barbour$Ref25 <- sapply(ref.df[, metric_col.1:ncol(ref.df)], quantile, 0.25, na.rm = TRUE)
    barbour$Ref50 <- sapply(ref.df[, metric_col.1:ncol(ref.df)], quantile, 0.50, na.rm = TRUE)
    barbour$Ref75 <- sapply(ref.df[, metric_col.1:ncol(ref.df)], quantile, 0.75, na.rm = TRUE)
    barbour$DEG25 <- sapply(deg.df[, metric_col.1:ncol(deg.df)], quantile, 0.25, na.rm = TRUE)
    barbour$DEG50 <- sapply(deg.df[, metric_col.1:ncol(deg.df)], quantile, 0.50, na.rm = TRUE)
    barbour$DEG75 <- sapply(deg.df[, metric_col.1:ncol(deg.df)], quantile, 0.75, na.rm = TRUE)
  }
  
  if(ncol(ref.df) == metric_col.1){
    barbour$Ref25 <- quantile(ref.df[, metric_col.1], 0.25, na.rm = TRUE)
    barbour$Ref50 <- quantile(ref.df[, metric_col.1], 0.50, na.rm = TRUE)
    barbour$Ref75 <- quantile(ref.df[, metric_col.1], 0.75, na.rm = TRUE)
    barbour$DEG25 <- quantile(deg.df[, metric_col.1], 0.25, na.rm = TRUE)
    barbour$DEG50 <- quantile(deg.df[, metric_col.1], 0.50, na.rm = TRUE)
    barbour$DEG75 <- quantile(deg.df[, metric_col.1], 0.75, na.rm = TRUE)
  }
  #============================================================================
  #Round the metric quartiles to two decimal places
  barbour[, -1] <- round(barbour[, -1], 2) #the "-1" excludes column 1
  
  barbour$DISTURBANCE <- ifelse(barbour$Ref50 > barbour$DEG50, "DECREASE",
                                ifelse(barbour$Ref50 < barbour$DEG50, "INCREASE",
                                       "EQUAL"))
  #============================================================================
  # Scoring from Barbour et al. 1996
  # 0 = The median of both ref.df and DEG plots overlap the interquartile
  # range of the other category.
  # 1 = One median (either ref.df or DEG) overlaps the interquartile range
  # of the other category.
  # 2 = The interquartiles overlap but neither median overlaps with the
  # interquartile range of the other category.
  # 3 = The interquartile ranges do not overlap.
  barbour$SENSITIVITY <- ifelse((barbour$Ref50 <= barbour$DEG75 &
                                   barbour$Ref50>=barbour$DEG25 &
                                   barbour$DEG50<=barbour$Ref75 &
                                   barbour$DEG50>=barbour$Ref25), 0,
                                ifelse((barbour$Ref50<=barbour$DEG75 &
                                          barbour$Ref50>=barbour$DEG25 |
                                          barbour$DEG50<=barbour$Ref75 &
                                          barbour$DEG50>=barbour$Ref25), 1,
                                       ifelse((barbour$Ref25<=barbour$DEG75 &
                                                 barbour$Ref25>=barbour$DEG25 |
                                                 barbour$Ref75<=barbour$DEG75 &
                                                 barbour$Ref75>=barbour$DEG25), 2, 3)))
  
  final.df <- barbour[, c("METRICS", "DISTURBANCE", "SENSITIVITY")]
  final.df$METRICS <- as.character(final.df$METRICS)
  
  return(final.df)
}

#==============================================================================
#'Barbour Sensitivity Score
#'
#'@param barbour.df = data frame of metrics scored using the Barbour et al. 1996 method
#'for testing metric sensitivity
#'@param score = a number 0 - 3 indicating which sensitivity score will be
#'represented by the new data frame
#'@return A data frame representing one sensitivity score from the
#' Barbour et al. 1996 method
#'@export
#'
barbour_sensitivity <- function(barbour.df, score){
  b.sensitivity <- by(barbour.df, barbour.df$Barbour_Score, FUN = print)
  b.score <- if(score == 3){
    b.sensitivity$'3'
  }else{
    if(score == 2){
      b.sensitivity$'2'
    }else{
      if(score == 1){
        b.sensitivity$'1'
      }else{
        if(score == 0){
          b.sensitivity$'0'
        }
      }
    }
  }
  
  return(data.frame(b.score))
}