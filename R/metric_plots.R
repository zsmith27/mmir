#==============================================================================
# Plot Metrics
#==============================================================================
#'Metric Plots
#'
#'@param metrics.df = data frame of metric values for each station
#'@return Exports a pdf of metric box-and-whisker plots into your working 
#'directory.
#'@export
#'
metric_plots <- function(metrics.df, condition.colname, first.metric,
                         order.condition, include.title = TRUE, name.pdf) {
  metric_col.1 <- which(names(metrics.df) %in% first.metric)
  metrics.cols <- names(metrics.df[, metric_col.1:ncol(metrics.df)])
  
  
  if (is.null(order.condition)) {
    metrics.df[, condition.colname] <- factor(metrics.df[, condition.colname])
  } else {
    metrics.df[, condition.colname] <- factor(metrics.df[, condition.colname],
                                             levels = order.condition)
  }
  
  #==============================================================================
  # Colorblind pallete
  cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  num.levels <- length(levels(metrics.df[, condition.colname]))
  if(num.levels == 2) sub.cbp <- cb_palette[c(4, 7)]
  if(num.levels == 3) sub.cbp <- cb_palette[c(4, 5, 7)]
  if(num.levels == 4) sub.cbp <- cb_palette[c(4, 5, 2, 7)]
  if(num.levels == 5) sub.cbp <- cb_palette[c(4, 5, 2, 7)]
  if(num.levels == 6) sub.cbp <- cb_palette[1:6]
  if(num.levels == 7) sub.cbp <- cb_palette[1:7]
  if(num.levels == 8) sub.cbp <- cb_palette[1:8]
  
  #==============================================================================
  plots <- lapply(metrics.cols, function(metric){
    p <- ggplot2::ggplot(metrics.df, ggplot2::aes_string(condition.colname, metric)) +
      ggplot2::stat_boxplot(geom ='errorbar', width = 0.5) + 
      ggplot2::geom_boxplot(width = 0.7) + #ggplot2::aes_string(fill = condition.colname)
      #ggplot2::scale_fill_manual(values = sub.cbp) +
      ggplot2::theme_bw() + 
      ggplot2::guides(fill = FALSE) +
      ggplot2::theme(panel.border = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(colour = "black"),
                     plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
    if(include.title == TRUE){
      p <- p + ggplot2::labs(title = metric) + 
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 18))
    } 
  })
  
  ml <- gridExtra::marrangeGrob(plots, nrow = 3, ncol = 2)
  ggplot2::ggsave(paste0(name.pdf, ".pdf"), ml, height = 11, width = 7.5)
  
}