
# <b>catsnip</b> ๐<a href="https://github.com/csh01470/catsnip"><img src="man/figures/logo.png" align="right" height="240"></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/catsnip)](https://cran.r-project.org/package=catsnip)
[![R-CMD-check](https://github.com/csh01470/catsnip/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/csh01470/catsnip/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

`catsnip` is a package that wraps `catboost` models in `parsnip` format.

This package is based on `treesnip` package and provides following
functions.

- Attach `catboost` model to `boost_tree()` function

- Install `catboost` package through `install_catboost()` function

- Avoid conflict between `bonsai` package and `catsnip` package during
  stacking model

## Documents

For a detailed explanation of `catboost` and `treesnip`, see links
below.

- [Catboost](https://catboost.ai/docs/)

- [treesnip](https://curso-r.github.io/treesnip/)

## Installation

Since `catsnip` is not listed on CRAN, so use `install_github()`
function.

``` r
devtools::install_github(repo="csh01470/catsnip")
```

You can also install `catboost` released version with,

``` r
catsnip::install_catboost() 
```

To install another version(ex: `0.16.5`), Use `version` parameter.

## Roadmaps

- [ ] Support `GPU` processing

- [ ] Optimization of parameters for `catboost` model and `boost_tree()`
  function

- [ ] Extract SHAP value by interworking with `fastshap` package

## Contributing

No matter your current skills itโs possible to contribute to `catsnip`
development.

See the contributing guide for more information.
