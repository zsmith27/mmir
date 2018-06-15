spread_condition  <- function(x, metric.col, condition.col, value.col, ref.cond, deg.cond) {
  metric.col <- rlang::enquo(metric.col)
  value.col <- rlang::enquo(value.col)
  condition.col <- rlang::enquo(condition.col)
  ref.col.name <- rlang::sym(paste("ref", rlang::quo_name(value.col), sep = "_"))
  deg.col.name <- rlang::sym(paste("deg", rlang::quo_name(value.col), sep = "_"))

  x %>%
    select(!!metric.col, !!condition.col, !!value.col) %>%
    spread(!!condition.col, !!value.col) %>%
    rename(!!ref.col.name := !!rlang::sym(ref.cond),
           !!deg.col.name := !!rlang::sym(deg.cond))
}

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
    summarize(values = list(!!value.col),
              total = length(!!value.col),
              quantiles = list(quantile(!!value.col, probs = seq(0, 1, by = 0.01), na.rm = TRUE)),
              quant25 = quantile(!!value.col, 0.25, na.rm = TRUE),
              median = quantile(!!value.col, 0.50, na.rm = TRUE),
              quant75 = quantile(!!value.col, 0.75, na.rm = TRUE)) %>%
    ungroup()


  final.df <- left_join(
    spread_condition(metrics.df, !!metric.col, !!condition.col, values, ref.cond, deg.cond),
    spread_condition(metrics.df, !!metric.col, !!condition.col, total, ref.cond, deg.cond),
    by = rlang::quo_name(metric.col)) %>%
    left_join(spread_condition(metrics.df, !!metric.col, !!condition.col, median, ref.cond, deg.cond),
              by = rlang::quo_name(metric.col)) %>%
    left_join(spread_condition(metrics.df, !!metric.col, !!condition.col, quant25, ref.cond, deg.cond),
              by = rlang::quo_name(metric.col)) %>%
    left_join(spread_condition(metrics.df, !!metric.col, !!condition.col, quant75, ref.cond, deg.cond),
              by = rlang::quo_name(metric.col)) %>%
    left_join(spread_condition(metrics.df, !!metric.col, !!condition.col, quantiles, ref.cond, deg.cond),
              by = rlang::quo_name(metric.col))

  final.df <- final.df %>%
    mutate(disturbance = case_when(
      ref_median > deg_median ~ "decrease",
      ref_median < deg_median ~ "increase",
      ref_median == deg_median ~ "equal",
      TRUE ~ "ERROR"
    ),
    total = ref_total + deg_total)

  return(final.df)
}





barbour <- function(prep.sensitivity, metric.col) {
  metric.col <- rlang::enquo(metric.col)

  prep.sensitivity %>%
    mutate(barbour = case_when(
      ref_median <= deg_quant75 & ref_median >= deg_quant25 &
        deg_median <= ref_quant75 & deg_median >= ref_quant25 ~ 0,
      ref_median <= deg_quant75 & ref_median >= deg_quant25  ~ 1,
      deg_median <= ref_quant75 & deg_median >= ref_quant25 ~ 1,
      ref_quant25 <= deg_quant75 & ref_quant25 >= deg_quant25 ~ 2,
      ref_quant75 <= deg_quant75 & ref_quant75 >= deg_quant25 ~ 2,
      TRUE ~ 3)) %>%
    select(!!metric.col, barbour)
}

de_orginal <- function(x, metric.col) {
  metric.col <- rlang::enquo(metric.col)

  x %>%
    group_by(!!metric.col) %>%
    mutate(de = case_when(
      disturbance == "decrease" ~ sum((unlist(deg_values) < ref_quant25)) / deg_total * 100,
      disturbance == "equal" ~ as.double(0),
      disturbance == "increase"  ~ sum(unlist(deg_values) > ref_quant75) / deg_total * 100,
      TRUE ~ as.double(10 ^ 6)
    ),
    de_thresh = case_when(
      disturbance == "decrease" ~ ref_quant25,
      disturbance == "equal" ~ as.double(0),
      disturbance == "increase"  ~ ref_quant75,
      TRUE ~ as.double(10 ^ 6)
    )) %>%
    ungroup() %>%
    select(!!metric.col, de_thresh, de)
}

bde_dev <- function(x, metric.col) {
  metric.col <- rlang::enquo(metric.col)

 test <-  x %>%
    group_by(!!metric.col) %>%
    mutate(pthresh = list(seq(ref_median, deg_median,
                              by = -1* ((ref_median - deg_median) / (25))))) %>%
    ungroup() %>%
    mutate(sel_vec = purrr::map(seq(nrow(.)), function(row.i) {
      slice(., row.i) %>%
        select_thresh(pthresh)
    })) %>%
    rowwise() %>%
    mutate(bde_thresh = sel_vec[[1]][[1]][1],
           bde = sel_vec[[1]][[1]][3]) %>%
    ungroup() %>%
    select(!!metric.col, bde_thresh, bde)
}

test2 <- test %>%
  mutate(bde_thresh = unlist(sel_vec)[1],
         bde = unlist(sel_vec)[3]) %>%
  ungroup() %>%
  select(!!metric.col, bde_thresh, bde)
test$sel_vec[[1]][[1]][[3]]

metric.col <- rlang::quo(metric)
value.col <- rlang::quo(value)
condition.col <- rlang::quo(lake)
ref.cond = "caz"
deg.cond = "onon"
method = "all"
sensitivity_dev <- function() {
  prep.df <- prep_sensitivity(metrics.long, !!metric.col, !!value.col, !!condition.col, ref.cond, deg.cond)

  final.df <- left_join(select(prep.df, !!metric.col, disturbance),
                        barbour(prep.df, !!metric.col),
                        by = rlang::quo_name(metric.col)) %>%
    left_join(de_orginal(prep.df, !!metric.col),
              by = rlang::quo_name(metric.col)) %>%
    left_join(bde_dev(prep.df, !!metric.col),
              by = rlang::quo_name(metric.col))

  final.df <- arrange(final.df, desc(bde))
  return(final.df)
}


less.than.col <- rlang::quo(deg_values)
greater.than.col <- rlang::quo(ref_values)

test_thresh <- function(x, less.than.col, greater.than.col, thresh) {
  less.than.col <- rlang::enquo(less.than.col)
  greater.than.col <- rlang::enquo(greater.than.col)

  final.df <- x %>%
    mutate(group1_pct = sum((unlist(!!less.than.col) < thresh)) / length(unlist(!!less.than.col)) * 100,
           group2_pct = sum((unlist(!!greater.than.col) > thresh)) / length(unlist(!!greater.than.col)) * 100,
           bal_factor = abs(group1_pct - group2_pct),
           bde = (sum(group1_pct, group2_pct) / 2 * 100) - bal_factor,
           bde_thresh = thresh) %>%
    ungroup() #%>%
  #select(bde_thresh, bde)

  list(final.df$bde_thresh, final.df$bde)

}

select_thresh <- function(x, thresh.col) {
  thresh.col <- rlang::enquo(thresh.col)
  test4 <- x %>%
    #head(1) %>%
    mutate(bde_list = list(purrr::map(unlist(!!thresh.col), function(thresh.i) {
      if (disturbance == "decrease") {
        test_thresh(x = ., less.than.col = deg_values, greater.than.col = ref_values, thresh = thresh.i)
      } else if(disturbance == "increase") {
        test_thresh(x = ., less.than.col = ref_values, greater.than.col = deg_values, thresh = thresh.i)
      } else if(disturbance == "equal") list(ref_median, 50.00)
    }))) %>%
    mutate(bde_vec = list(map_dbl(bde_list[1][[1]],`[[`, 2))) %>%
    mutate(sel_vec = bde_list[1][[1]][median(which(unlist(bde_vec) == max(unlist(bde_vec))))])

  test4$sel_vec
}
blah <- x %>%
  mutate(test = purrr::map(seq(nrow(.)), function(metric.i) {
    test5 %>%
   slice(1) %>%
      select_thresh(pthresh)
  }))

select_thresh(x, pthresh)
test6 <- list(sapply(test4$bde_list[1][[1]],`[[`, 2))

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
