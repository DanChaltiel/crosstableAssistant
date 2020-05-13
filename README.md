
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crosstable

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/crosstableAssistant)](https://CRAN.R-project.org/package=crosstable)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

Crosstable is a package centered on a single function, `crosstable`,
which easily computes descriptive statistics on datasets. You can learn
about it on its [dedicated
page](https://github.com/DanChaltiel/crosstable).

CrosstableAssistant is an RShiny application, designed as an RStudio
addin, which makes the use of `crosstable` much easier by providing a
graphical interface for all its parameters.

## Installation

``` r
install.packages("devtools")
devtools::install_github("DanChaltiel/crosstable")
devtools::install_github("DanChaltiel/crosstableAssistant")
```

## Usage

``` r
library(crosstable)

my_dataset = crosstable::mtcars2
crosstableAssistant(my_dataset)
```

Alternatively, if you are using RStudio, you can use the “Crosstable
Assistant” addin. Just select your dataset in the “source” panel and it
will be loaded inside the assistant.

## Acknowledgement

This `crosstableAssistant` package is highly inspired by `dreamRs`’
amazing addin `esquisse` ([link](https://github.com/dreamRs/esquisse)),
which is designed to easily build plots with ggplot2. If you never did,
you should definitely give it a try.

Thanks `dreamRs`\!
