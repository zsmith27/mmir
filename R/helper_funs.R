
calc_quantiles <- function(metrics.df, quantiles) {
  quantiles.100 <- 100 * quantiles.vec
  lapply(1:ncol(metrics.df), function(i) {
    final.df <- quantile(metrics.df[, i][[1]], probs = quantiles.vec, na.rm = TRUE) %>%
      t() %>%
      data.frame()
    names(final.df) <- paste("quant", 100 * quantiles.vec, sep = "_")
    final.df <- final.df %>%
      dplyr::mutate(metric = names(metrics.df[, i])) %>%
      dplyr::select(metric, dplyr::everything())
  }) %>%
    dplyr::bind_rows() %>%
    tidyr::gather(quantile, quant_val, quant_25:quant_75)
}
