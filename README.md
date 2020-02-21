
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mmir <img src='man/figures/logo.png' align="right" height="115" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/mmir)](https://CRAN.R-project.org/package=mmir)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/zsmith27/mmir.svg?branch=master)](https://travis-ci.org/zsmith27/mmir)
[![Codecov test
coverage](https://codecov.io/gh/zsmith27/mmir/branch/master/graph/badge.svg)](https://codecov.io/gh/zsmith27/mmir?branch=master)
<!-- badges: end -->

It is common practice for regulatory agencies to use biological
communities, such as macroinvertebrate, fish, or diatoms, to evaluate
water quality condition. Multi-Metric Indices (MMI) or Indices of Biotic
Integrity are the standard models for biological assessment, yet there
is no package in R specifically dedicated to calculating common or
exploratory biological community metrics. The mmir (Multi-Metrics
Indices in R) package is intended to simplify and standardize the
production of biological community metrics within R. The package
provides a straightforward syntax for calculating richness, diversity
(e.g., Shannon-Wiener and Simpsons), relative abundance, dominance,
functional feeding group, habit, and tolerance value metrics. Building
on this base functionality for calculating these individual metrics,
there are built in functions for iterating through each metric type to
provide a large suite of exploratory metrics; for example, with a single
call relative abundance metrics for all orders, families, genera, and
species in a dataset can be generated. These exploratory metrics must be
reviewed for ecological context, but the limitation of calculating new
metrics or evaluating new aspects of the community has been
substantially reduced.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("zsmith27/mmir")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(mmir)
## basic example code
```
