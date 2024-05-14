
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phslookups

<!-- badges: start -->
<!-- badges: end -->

The goal of phslookups is to provide helper functions to easily access
and use lookups which have been provided for analysis.

## Installation

You can install the latest version of phslookups from
[GitHub](https://github.com) with:

``` r
# Not yet added
remotes::install_github("moohan/phslookups")
```

## Example

Load the Scottish Postcode Directory

``` r
library(phslookups)

spd <- get_spd()
#> ℹ Using "Scottish_Postcode_Directory_2024_1.parquet".
```
