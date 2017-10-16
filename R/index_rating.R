#==============================================================================
# Rate the Index
#==============================================================================
#'Index Rating
#'
#'@param scores.df = a long data frame format containing the index scores.
#'@param index.colname = the name of the column containing the index scores.
#'@param condition.colname = the name of the column containing the disturbance
#'gradient classes.
#'@param ref.cond = the character string that refers to the Reference class in
#'the disturbance gradient column (condition.colname).
#'@return A wrapper function that allows you to select your scoring methodology
#'and a metric sensitivity threshold.
#'@export

index_rating <- function(scores.df, index.colname, condition.colname = NULL,
                         ref.cond = NULL){
  # Subset scores to focus on reference samples.
  ref.df <- scores.df[scores.df[, condition.colname] %in% ref.cond, ]
  # Find the 50th, 25th, and 10th percentiles of the reference distribution.
  percent.df <- as.data.frame(t(quantile(ref.df[, index.colname],
                                         c(0.50, 0.25, 0.10))))
  # Rename the columns.
  names(percent.df) <- c("PCT_50", "PCT_25", "PCT_10")
  # Calculate half the value of the Reference 10th percentile.
  percent.df$PCT_HALF_10 <- percent.df$PCT_10 / 2
  # Create a final data frame.
  final.df <- scores.df
  final.df$PCT_50  <- percent.df$PCT_50
  final.df$PCT_25  <- percent.df$PCT_25
  final.df$PCT_10  <- percent.df$PCT_10
  final.df$PCT_HALF_10  <- percent.df$PCT_HALF_10
  #----------------------------------------------------------------------------
  # Rate the samples base on the index scores.
  final.df$RATING <- ifelse(final.df[, index.colname] >= percent.df$PCT_50, "EXCELLENT",
                             ifelse(final.df[, index.colname] >= percent.df$PCT_25, "GOOD",
                                    ifelse(final.df[, index.colname] >= percent.df$PCT_10, "FAIR",
                                           ifelse(final.df[, index.colname] >= percent.df$PCT_HALF_10, "POOR",
                                                  ifelse(final.df[, index.colname] < percent.df$PCT_HALF_10, "VERY_POOR",
                                                         "ERROR")))))
  #----------------------------------------------------------------------------
  # End index_rating function.
  return(final.df)
}