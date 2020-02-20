## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("devtools", "DT")

## ---- eval=FALSE--------------------------------------------------------------
#  devtools::install_github("zsmith27/mmir", ref = "dev", force = TRUE, quiet = TRUE)

## -----------------------------------------------------------------------------
library(mmir)

## -----------------------------------------------------------------------------
data("onondaga", package = "mmir")

