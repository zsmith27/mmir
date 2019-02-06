# metrics.vec <- sensitivity.df %>%
#   filter(de >= 70) %>%
#   pull(metric)
#
# metric.sub <- metrics.wide %>%
#   select(metrics.vec)
#
# cor.df <- data.frame(cor(metric.sub), method = "spearman")
# cor.df$metric_y <- row.names(cor.df)
#
# long.df <- cor.df %>%
#   gather(metric_x, value, - metric_y) %>%
#   distinct()
