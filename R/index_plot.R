#==============================================================================
# Generate Index Box and Whisker Plot
#==============================================================================
#'Index Plot
#'
#'@param scores.df = data frame of metric values for each station
#'@param ref.cond = The site classification that represents better
#'environmental conditions.
#'@param deg.cond = The site classification that represents the degraded
#'environmental conditions.
#'@param method = the sensitivity function to be used during the assessment.
#'@return Determines the threshold at which a metric best categorizes
#'reference and degraded stations.
#'@import ggplot2
#'@export

index_plot <- function(index.df, condition.colname, condition.order = NULL,
                       index.colname = "INDEX", threshold, plot.title) {
  
  if (!is.null(condition.order)) {
    index.df[, condition.colname] <- factor(index.df[, condition.colname],
                                            levels = condition.order)
  }
  #----------------------------------------------------------------------------
  agg.df <- aggregate(index.df[, index.colname] ~ index.df[, condition.colname],
                      data = index.df, FUN = max)
  names(agg.df) <- c(condition.colname, "MAX")
  len.df <- aggregate(index.df[, index.colname] ~ index.df[, condition.colname],
                      data = index.df, FUN = length)
  names(len.df) <- c(condition.colname, "N")
  n.size <- merge(agg.df, len.df, by = condition.colname)
  #----------------------------------------------------------------------------
  final.plot <- ggplot(index.df, aes_string(condition.colname, index.colname)) +
    stat_boxplot(geom = 'errorbar', width = 0.4) + 
    geom_boxplot(notch = FALSE) +
    theme(panel.background = element_rect(fill = 'white', colour = 'white'), 
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          # text = element_text(size = 20),
          #axis.text.x = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
          axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
          plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm")) +
    labs(title = plot.title) +
    geom_text(data = n.size,
              #aes(y = MAX + 1, label = N),
              aes(y = 101, label = N),
              vjust = 0)
  
  if (max(index.df[, index.colname]) > 10) {
    #final.plot <- final.plot + ylim(0, max(n.size$MAX) + 1)
    final.plot <- final.plot + scale_y_continuous(limits = c(-2, 108),
                                                  expand = c(0, 0),
                                                  breaks = seq(0, 100, by = 25))
  } else {
    final.plot <- final.plot + ylim(0, max(index.df[, index.colname]) + 1)
  }
  
  if (!is.null(threshold)) {
    final.plot <- final.plot + 
      geom_hline(aes(yintercept = threshold),
                 size = 1.25, color = "red2", linetype = "dotted")
  }
  
  print(final.plot)
}


#==============================================================================
# Generate Index Box and Whisker Plot
#==============================================================================
#'Index Plot
#'
#'@param scores.df = data frame of metric values for each station
#'@param ref.cond = The site classification that represents better
#'environmental conditions.
#'@param deg.cond = The site classification that represents the degraded
#'environmental conditions.
#'@param method = the sensitivity function to be used during the assessment.
#'@return Determines the threshold at which a metric best categorizes
#'reference and degraded stations.
#'@import ggplot2
#'@export

index_plot2 <- function(index.df, condition.colname, condition.order = NULL,
                       index.colname = "INDEX", threshold, plot.title, rating.df) {
  
  if (!is.null(condition.order)) {
    index.df[, condition.colname] <- factor(index.df[, condition.colname],
                                            levels = condition.order)
  }
  #----------------------------------------------------------------------------
  agg.df <- aggregate(index.df[, index.colname] ~ index.df[, condition.colname],
                      data = index.df, FUN = max)
  names(agg.df) <- c(condition.colname, "MAX")
  len.df <- aggregate(index.df[, index.colname] ~ index.df[, condition.colname],
                      data = index.df, FUN = length)
  names(len.df) <- c(condition.colname, "N")
  n.size <- merge(agg.df, len.df, by = condition.colname)
  #----------------------------------------------------------------------------
  final.plot <- ggplot(index.df, aes_string(condition.colname, index.colname)) +
    stat_boxplot(geom = 'errorbar', width = 0.4) + 
    geom_boxplot(notch = FALSE) +
    theme(panel.background = element_rect(fill = 'white', colour = 'white'), 
          axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          # text = element_text(size = 20),
          #axis.text.x = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
          axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
          plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm")) +
    labs(title = plot.title) +
    geom_text(data = n.size, aes(y = MAX + 1, label = N), vjust = 0)
  
  if (max(index.df[, index.colname]) > 10) {
    #final.plot <- final.plot + ylim(0, max(n.size$MAX) + 1)
    final.plot <- final.plot + scale_y_continuous(limits = c(-2, 108),
                                                  expand = c(0, 0),
                                                  breaks = seq(0, 100, by = 25))
  } else {
    final.plot <- final.plot + ylim(0, max(index.df[, index.colname]) + 1)
  }
  
  if (!is.null(threshold)) {
    final.plot <- final.plot + 
      geom_hline(aes(yintercept = threshold),
                 size = 1.25, color = "red2", linetype = "dotted")
  }
  
  print(final.plot)
}