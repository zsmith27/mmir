
metric.col <- rlang::quo(metric)
value.col <- rlang::quo(value)
condition.col <- rlang::quo(lake)
ref.cond = "caz"
deg.cond = "onon"
method = "all"

prep_sensitivity <- function(metrics.long, metric.col, value.col, condition.col, ref.cond, deg.cond) {
  metric.col <- rlang::enquo(metric.col)
  value.col <- rlang::enquo(value.col)
  condition.col <- rlang::enquo(condition.col)

  metrics.df <- metrics.long %>%
    select(!!metric.col, !!value.col, !!condition.col) %>%
    filter((!!condition.col) %in% c(ref.cond, deg.cond))

  metrics.df <- metrics.df %>%
    group_by(!!metric.col, !!condition.col) %>%
    summarize(quantiles = list(quantile(!!value.col, probs = seq(0, 1, by = 0.01), na.rm = TRUE)),
              quant_25 = quantile(!!value.col, 0.25, na.rm = TRUE),
              median = quantile(!!value.col, 0.50, na.rm = TRUE),
              quant_75 = quantile(!!value.col, 0.75, na.rm = TRUE)) %>%
    ungroup()

  metrics.disturb <- metrics.df %>%
    select(!!metric.col, !!condition.col, median) %>%
    spread(!!condition.col, median) %>%
    mutate(disturbance = case_when(
      (!!rlang::sym(ref.cond)) > (!!rlang::sym(deg.cond)) ~ "decrease",
      (!!rlang::sym(ref.cond)) < (!!rlang::sym(deg.cond)) ~ "increase",
      (!!rlang::sym(ref.cond)) == (!!rlang::sym(deg.cond)) ~ "equal",
      TRUE ~ "ERROR"
    )) %>%
    select(!!metric.col, disturbance)

  final.df <- metrics.df %>%
    left_join(metrics.disturb, by = rlang::quo_name(metric.col))

  return(final.df)
}

barbour <- function() {
  final.df2 <- final.df %>%
    select(!!metric.col, !!condition.col, quant_25, median, quant_75) %>%
    mutate(condition = factor(!!condition.col, levels = c(ref.cond, deg.cond))) %>%
    group_by(!!metric.col, !!condition.col) %>%
    mutate(quant_25_diff = diff(quant_25))
    mutate(barbour = case_when(

    ))
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
}
sensitivity <- function(metrics.long, first.metric, condition.colname,
                        ref.cond = "REF", deg.cond = "DEG",
                        method = "BDE"){



  #============================================================================
  if(method %in% c("BARBOUR", "ALL")){
    barb.df <- barbour(metrics.df, first.metric, ref.df, deg.df)
  }

  if(method %in% c("DE", "ALL")){
    de.df <- original_de(deg.df, quant.df, first.metric)
  }

  if(method %in% c("BDE", "ALL")){
    bde.df <- balanced_ce(metrics.df, first.metric, quant.df,
                          condition.colname,
                          ref.cond, deg.cond,
                          ref.df, deg.df, quant.ref)
  }

  if(method %in% "ALL"){
    names(barb.df)[names(barb.df) %in% "SENSITIVITY"] <- "BARBOUR_SENSITIVITY"
    names(de.df)[names(de.df) %in% "SENSITIVITY"] <- "DE_SENSITIVITY"
    names(bde.df)[names(bde.df) %in% "SENSITIVITY"] <- "BDE_SENSITIVITY"
    final.df <- plyr::join_all(list(barb.df, de.df, bde.df), c("METRICS", "DISTURBANCE"))
  } else {
    if(exists("barb.df")) final.df <- barb.df
    if(exists("de.df")) final.df <- de.df
    if(exists("bde.df")) final.df <- bde.df
  }


  return(final.df)
}
