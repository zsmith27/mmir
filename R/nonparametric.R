#==============================================================================
#==============================================================================
# Title: Nonparametric Tests
# Author: Zachary M. Smith
# Organization: ICPRB
# Email: zsmith@icprb.org
# Date: 3/08/2017
# Purpose: Script to summarize Wilcoxon Rank Sum, kruskal-wallis, and dunn's
#          nonparametric tests.
#==============================================================================
#==============================================================================
#'Wilcoxon Rank Sum Test Table
#'
#'@param wide.df = a data frame of samples to be scored.
#'@param first.metric = the first metric column that appears in your wide
#'data frame when reading from left to right. It is assumed that all columns
#'to the right of the first metric column are metrics. Any other columns will
#'result in an error or, if the column is numeric, it will be treated as if
#'it were a metric.
#'@param groub.by = specify the column upon which the nonparametric assessment
#'will be performed.
#'@return Summarize the wilcoxon Rank Sum results into a clean table.
#'@export

wilcox_tbl <- function(wide.df, first.metric, group.by){
  metric_col.1 <- which(names(wide.df) %in% first.metric)
  
  nt <- lapply(wide.df[, metric_col.1:ncol(wide.df)], function(x) wilcox.test(x ~ wide.df[, group.by]))
  df <- data.frame(matrix(unlist(nt), nrow =  length(nt), byrow = TRUE), stringsAsFactors = FALSE)
  
  new.df <- data.frame(df[, 1:2])
  colnames(new.df) <- c("Wilcox_Statistic", "Wilcox_p_value")
  new.df$Wilcox_Statistic <- round(as.numeric(new.df$Wilcox_Statistic), digits = 6)
  new.df$Wilcox_p_value <- round(as.numeric(new.df$Wilcox_p_value), digits = 6)
  
  metrics_names.df <- data.frame(names(wide.df[, metric_col.1:ncol(wide.df)]))
  colnames(metrics_names.df) <- "METRIC"
  final.df <- cbind(metrics_names.df, new.df)
  return(final.df)
}

#==============================================================================
#'Kruskal-Wallis Rank Sum Test Table
#'
#'@param wide.df = a data frame of samples to be scored.
#'@param first.metric = the first metric column that appears in your wide
#'data frame when reading from left to right. It is assumed that all columns
#'to the right of the first metric column are metrics. Any other columns will
#'result in an error or, if the column is numeric, it will be treated as if
#'it were a metric.
#'@param groub.by = specify the column upon which the nonparametric assessment
#'will be performed.
#'@return Summarize the Kruskal-Wallis Rank Sum results into a clean table.
#'@export

kruskal_tbl <- function(wide.df, first.metric, group.by){
  metric_col.1 <- which(names(metrics.df) %in% first.metric)
  
  nt <- lapply(wide.df[, metric_col.1:ncol(wide.df)], function(x) kruskal.test(x ~ wide.df[, group.by]))
  df <- data.frame(matrix(unlist(nt), nrow =  length(nt), byrow = TRUE),stringsAsFactors = FALSE)
  colnames(df) <- c("kw_Statistic", "df", "kw_p_value", "Test", "Description")
  
  new.df <- df[, 1:3]
  new.df$kw_Statistic <- round(as.numeric(new.df$kw_Statistic), digits = 6)
  new.df$kw_p_value <- round(as.numeric(new.df$kw_p_value), digits = 6)
  
  metric_names.df <- data.frame(colnames(wide.df[, metric_col.1:ncol(wide.df)]))
  colnames(metric_names.df) <- "METRIC"
  final.df <- cbind(metric_names.df, new.df)
  return(final.df)
}

#==============================================================================
#'Dunn's Test Table
#'
#'@param wide.df = a data frame of samples to be scored.
#'@param first.metric = the first metric column that appears in your wide
#'data frame when reading from left to right. It is assumed that all columns
#'to the right of the first metric column are metrics. Any other columns will
#'result in an error or, if the column is numeric, it will be treated as if
#'it were a metric.
#'@param groub.by = specify the column upon which the nonparametric assessment
#'will be performed.
#'@return Summarize the Dunn's Test results into a clean table.
#'@export

dunn_tbl <- function(wide.df, first.metric, group.by){
  metric_col.1 <- which(names(metrics.df) %in% first.metric)
  
  #nt <- lapply(names(wide.df[, metric_col.1:ncol(wide.df)]), function(x){
  dunn.list <- list()
  for (x in names(wide.df[, metric_col.1:ncol(wide.df)])){
    print(x)
    metric.vec <- wide.df[, x]
    test.min.max <- lapply(unique(wide.df[, group.by]), function(y){
      group.wide <- wide.df[wide.df[, group.by] == y, x]
      group.min <- min(group.wide, na.rm = TRUE)
      group.max <- max(group.wide, na.rm = TRUE)
      group.max - group.min
    })
    
    test.min.max <- data.frame(TEST = test.min.max)
    
    if (any(test.min.max[1, ] == 0)){
      next
    } else {
      dunn.list[[x]] <- dunn.test::dunn.test(metric.vec , wide.df[, group.by], kw = TRUE, label = TRUE)
    }
  }
  nt <- dunn.list
  
  #})
  df <- data.frame(names(nt), matrix(unlist(nt),
                                     ncol = 1 + 4 * length(unique(nt[[first.metric]]$comparisons)),
                                     byrow = T), stringsAsFactors = FALSE)
  
  colnames(df) <- c("Metric", "Dunn_Chi2", paste("Z", nt[[first.metric]]$comparisons, sep="_"),
                    paste("Dunn_P_Value", nt[[first.metric]]$comparisons, sep="_"),
                    paste("P_Adjust", nt[[first.metric]]$comparisons, sep = "_"))
  
  grepped <- grep("Dunn_P_Value", colnames(df))
  dunn_pvalue <- df[, c(1, grepped)]
  if(length(grepped) > 1){
    dunn_pvalue[, 2:ncol(dunn_pvalue)] <- apply(dunn_pvalue[, 2:ncol(dunn_pvalue)], 2,
                                                function(x) as.numeric(as.character(x)))
  }else{
    if(z < 0){
      next
    }else{
      dunn_pvalue[, 2:ncol(dunn_pvalue)] <- as.numeric(dunn_pvalue[, 2:ncol(dunn_pvalue)])
      
    }
  }
  
  
  dunn_pvalue[,2:ncol(dunn_pvalue)] <- round(dunn_pvalue[,2:ncol(dunn_pvalue)], digits = 4)
  
  names(dunn_pvalue) <- toupper(names(dunn_pvalue))
  return(dunn_pvalue)
}

#==============================================================================
#'Nonparametric Test Table
#'
#'@param wide.df = a data frame of samples to be scored.
#'@param first.metric = the first metric column that appears in your wide
#'data frame when reading from left to right. It is assumed that all columns
#'to the right of the first metric column are metrics. Any other columns will
#'result in an error or, if the column is numeric, it will be treated as if
#'it were a metric.
#'@param groub.by = specify the column upon which the nonparametric assessment
#'will be performed. If the group.by column contains less than 2 groups, then
#'an error will be returned. If the group.by column contains 2 groups, then
#'a Wilcoxon Rank Sum Test will be performed. If the group.by column contains
#'more than 2 columns, then a Kruskal-Wallis Test and a post hoc Dunn's test
#'will be performed.
#'@return Nonparametric test results are summarized into a clean table.
#'@export

nonpar_tbl <- function(wide.df, first.metric, group.by){
  group.length <- length(unique(wide.df[, group.by]))
  if (group.length == 2) {
    final.df <- wilcox_tbl(wide.df, first.metric, group.by)
  } else {
    if (group.length > 2) {
      kruskal.df <- kruskal_tbl(wide.df, first.metric, group.by)
      dunn.df <- dunn_tbl(wide.df, first.metric, group.by)
      final.df <- merge(kruskal.df, dunn.df, by = "METRIC", all = TRUE)
      final.df[is.na(final.df)] <- "z-score < 0"
    } else {
      stop("The specified group.by column must contain at least two groups.")
    }
  }
  
  return(final.df)
}


