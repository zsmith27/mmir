#==============================================================================
#'
#'Chunk Sensitivity
#'
#'@param metrics.df = data frame of metric values for each station with site
#'a column of site classes defined by environmental variables.
#'@param upper.class = the site class that represents the better condition.
#'@param lower.class = the site class that represents the poorer condition.
#'@param method = the sensitivity function to be used during the assessment.
#'@return Determines the threshold at which a metric best categorizes
#'two defined environmental conditions.
#'@export

seq_sensitivity <- function(metrics.df, condition.colname, ref.cond = "REF", deg.cond = "DEG", method){
  
  
  metrics.list <- break.me(metrics.df, 100, 6)
  #============================================================================
  datalist = list()
  for(j in 1:length(metrics.list)){
    sub.metrics <- metrics.list[[j]]
    de.thresh <- sensitivity(sub.metrics, upper.class, lower.class, method)
    datalist[[j]] <- de.thresh
  }
  #============================================================================
  final.df <- do.call(rbind, datalist)
  
  return(final.df)
  
}

#==============================================================================
#'
#'Break Me
#'
#'@param metrics.df = data frame of metric values for each station with site
#'a column of site classes defined by environmental variables.
#'@param breaks = intervals at which columns will be subdivided.
#'@param site.info.thesh = the last column indicating site information. The next
#'column should represent metric values.
#'@return Subdivides a large data frame into multiple data frames that are
#' contained within a list.  Breaking a large data frame into multiple smaller
#' data frames will hopefully reduce the memory requirements.
#'@export

break.me <- function(metrics.df, breaks = 100, site.info.thresh = 6){
  seq.vec <- seq(0, 2000, breaks)
  check <- data.frame(NCOL = seq.vec[ncol(metrics.df) > seq.vec])
  
  datalist = list()
  for(i in check$NCOL){
    if(ncol(metrics.df) > i & ncol(metrics.df) < (i + breaks)){
      if(ncol(metrics.df) > breaks) sub.metrics <- metrics.df[, c(1:site.info.thresh, i:ncol(metrics.df))]
      if(ncol(metrics.df) <= breaks) sub.metrics <- metrics.df
    }else{
      if(i < breaks){
        sub.metrics <- metrics.df[ , 1:(breaks - 1)]
      } else {
        if(ncol(metrics.df) > breaks) sub.metrics <- metrics.df[, c(1:site.info.thresh, i:(i + (breaks - 1)))]
      }
    }
    datalist[[as.character(i)]] <- sub.metrics # add it to your list
  }
  return(datalist)
}
#==============================================================================