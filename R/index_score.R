#==============================================================================
# Generate Final Index Score
#==============================================================================
#'Index Score
#'
#'@param scores.df = data frame of metric values for each station
#'@param ref.cond = The site classification that represents better
#'environmental conditions.
#'@param deg.cond = The site classification that represents the degraded
#'environmental conditions.
#'@param method = the sensitivity function to be used during the assessment.
#'@return Determines the threshold at which a metric best categorizes
#'reference and degraded stations.
#'@export

index_score <- function(scores.df, first.metric, keep.metrics){
  # Identify the column number associated with the metric that
  # comes first when reading the columns from left to right.
  metric_col.1 <- which(names(scores.df) %in% first.metric)
  # Specify only the columns associated with sample information.
  keep.cols <- names(scores.df[, 1:(metric_col.1 - 1)])
  # Append the vector containing the selected metrics to
  # the vector of columns to keep.
  keep.cols <- c(keep.cols, keep.metrics)
  # Subset the scores.df to only include the specified columns.
  final.df <- scores.df[, keep.cols]
  # Find the mean value of the specified metric columns by row.
  final.df$INDEX <- apply(final.df[, keep.metrics], 1, mean)
  # End index_score function.
  return(final.df)
}
