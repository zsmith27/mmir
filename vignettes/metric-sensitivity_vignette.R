## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----load-packages------------------------------------------------------------
library(tidyr)
library(dplyr)
library(mmir)

## ----load-data----------------------------------------------------------------
data("nrsa_nap_metrics.df")

## ----pivot-longer-------------------------------------------------------------
long.df <- tidyr::pivot_longer(data = nrsa_nap_metrics.df,
                               cols = -c(uid, rt_nrsa_cat),
                               names_to = "metric",
                               values_to = "value")

## ----sensitivity--------------------------------------------------------------
sensitivity.df <- sensitivity(.dataframe = long.df,
                              .metric_col = metric,
                              .value_col = value,
                              .condition_col = rt_nrsa_cat,
                              .reference = "least disturbed",
                              .degraded = "most disturbed")

## ----sens-table, echo=FALSE---------------------------------------------------
sensitivity.df  %>% 
  knitr::kable()

