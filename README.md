
## `viabilitymetrics`: Seed Viability Calculations and Curve Fitting <img src="https://raw.githubusercontent.com/aravind-j/viabilitymetrics/master/inst/extdata/viabilitymetrics.png" align="right" alt="logo" width="173" height = "200" style = "padding: 10px; border: none; float: right;">

###### Version : [0.0.0.9100](https://aravind-j.github.io/viabilitymetrics/articles/Introduction.html#version-history); Copyright (C) 2017-2018: [ICAR-NBPGR](http://www.nbpgr.ernet.in/); License: [GPL2|GPL-3](https://www.r-project.org/Licenses/)

##### *Aravind, J., Radhamani, J., Vimala Devi, S., Jacob, S. R., and Kalyani Srinivasan*

ICAR-National Bureau of Plant Genetic Resources, New Delhi

-----

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.0.2-6666ff.svg)](https://cran.r-project.org/)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version-last-release/viabilitymetrics)](https://cran.r-project.org/package=viabilitymetrics)
<!-- [![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/viabilitymetrics?color=green)](https://CRAN.R-project.org/package=viabilitymetrics) -->
<!-- [![packageversion](https://img.shields.io/badge/Package%20version-0.2.3.3-orange.svg)](https://github.com/aravind-j/viabilitymetrics) -->
[![develVersion](https://img.shields.io/badge/devel%20version-0.0.0.9100-orange.svg)](https://github.com/aravind-j/viabilitymetrics)
<!-- [![GitHub Download Count](https://github-basic-badges.herokuapp.com/downloads/aravind-j/viabilitymetrics/total.svg)] -->
[![Project Status:
WIP](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Last-changedate](https://img.shields.io/badge/last%20change-2019--01--30-yellowgreen.svg)](/commits/master)
<!-- [![Rdoc](http://www.rdocumentation.org/badges/version/viabilitymetrics)](http://www.rdocumentation.org/packages/viabilitymetrics) -->
<!-- [![Zenodo DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.841963.svg)](https://doi.org/10.5281/zenodo.841963) -->
[![Analytics](https://pro-pulsar-193905.appspot.com/UA-116693474-1/welcome-page)](https://github.com/aravind-j/google-analytics-beacon)

-----

## Description

An implementation of viability equations of Ellis and Roberts (1980)
[doi:10.1093/oxfordjournals.aob.a085797\> and Mead and Grey (1999)
\<doi:10.1017/s0960258599000070](https://doi.org/10.1093/oxfordjournals.aob.a085797%3E%20and%20Mead%20and%20Grey%20\(1999\)%20%3Cdoi:10.1017/s0960258599000070),
for seed viability curve fitting and calculation of several seed
viability metrics such as storage period, final viability, storage
moisture content, storage temperature and days to loose one probit
viablity. The package further includes various conversions and
transformations associated with seed viability calculations.

## Installation

### Install development version from Github

``` r
devtools::install_github("aravind-j/viabilitymetrics")
```

## Detailed tutorial

For a detailed tutorial on how to used this package type:

``` r
browseVignettes(package = 'viabilitymetrics')
```

The vignette for the latest version is also available
[online](https://aravind-j.github.io/viabilitymetrics/articles/Introduction.html).

## What’s new

To know whats new in this version
type:

``` r
news(package='viabilitymetrics')
```

## Links

<!-- [CRAN page](https://cran.r-project.org/package=viabilitymetrics)  -->

[Github page](https://github.com/aravind-j/viabilitymetrics)

[Documentation website](https://aravind-j.github.io/viabilitymetrics/)

<!-- [Zenodo DOI](https://doi.org/10.5281/zenodo.841963) -->

## Citing `viabilitymetrics`

To cite the methods in the package use:

``` r
citation("viabilitymetrics")
```

``` 

To cite the R package 'viabilitymetrics' in publications use:

  Aravind, J., Radhamani, J., Vimala Devi, S., Jacob, S. R., and
  Kalyani Srinivasan (2019).  viabilitymetrics: Seed Viability
  Calculations and Curve Fitting. R package version 0.0.0.9100,
  https://aravind-j.github.io/viabilitymetrics/.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {viabilitymetrics: Seed Viability Calculations and Curve Fitting},
    author = {J. Aravind and J. Radhamani and S. {Vimala Devi} and Sherry Rachel Jacob and {Kalyani Srinivasan}},
    year = {2019},
    note = {R package version 0.0.0.9100},
    note = {https://aravind-j.github.io/viabilitymetrics/},
  }

This free and open-source software implements academic research by
the authors and co-workers. If you use it, please support the
project by citing the package.
```
