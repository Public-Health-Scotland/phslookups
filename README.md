
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phslookups

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of phslookups is to provide helper functions to easily access
and use lookups which have been provided for analysis.

## Installation

If you are internal to PHS, you can install the latest version of
phslookups from the internal Posit Package Manager.

``` r
# On the Posit Workbench
install.packages("phslookups")

# On RStudio Desktop it might be nessecary to specify the repository
install.packages("phslookups", repos = "https://ppm-prod.publichealthscotland.org/phs-github/latest")
```

To install the development version directly from GitHub.

``` r
remotes::install_github("Public-Health-Scotland/phslookups")
```

Note that phslookups will never be available on CRAN as it is intended
to be an internal-only package, and relies on lookup files on
internal-only drives.

## Available lookups

- The Scottish Postcode Directory - `get_spd()`
- HSCP Localities - `get_hscp_locality()`
- SIMD lookups - `get_simd_datazone()` and `get_simd_postcode()`

## Example

Load the latest available version of the full Scottish Postcode
Directory.

``` r
library(phslookups)

spd <- get_spd()
#> ℹ Using the latest available version:
#> "Scottish_Postcode_Directory_2026_1".
#> If you require an older version or for reproducibility purposes
#> please specify the version argument accordingly.
head(spd)
#> # A tibble: 6 × 95
#>   pc7     pc8     split_char pc_district pc_sector date_of_introduction
#>   <chr>   <chr>   <chr>      <chr>       <chr>     <date>              
#> 1 AB1 0AA AB1 0AA <NA>       AB1         AB1 0     1980-01-01          
#> 2 AB1 0AB AB1 0AB <NA>       AB1         AB1 0     1973-08-01          
#> 3 AB1 0AD AB1 0AD <NA>       AB1         AB1 0     1973-08-01          
#> 4 AB1 0AE AB1 0AE <NA>       AB1         AB1 0     1994-02-01          
#> 5 AB1 0AF AB1 0AF <NA>       AB1         AB1 0     1990-12-01          
#> 6 AB1 0AG AB1 0AG <NA>       AB1         AB1 0     1990-12-01          
#> # ℹ 89 more variables: date_of_deletion <date>, postcode_type <chr>,
#> #   pc_su_link <chr>, split_su_link <chr>, imputed <chr>, dpc <int>,
#> #   dpc_nr <int>, hhc <int>, grid_reference_easting <int>,
#> #   grid_reference_northing <int>, latitude <dbl>, longitude <dbl>,
#> #   split_indicator <chr>, ca2019 <chr>, ca2019name <chr>, ca2018 <chr>,
#> #   ca2011 <chr>, upc2024 <chr>, spr2021 <chr>, spc2021 <chr>, ew2022 <chr>,
#> #   hb2019 <chr>, hb2019name <chr>, hb2018 <chr>, hb2014 <chr>, hb2006 <chr>, …
```

Get a specific version of a file, great if you want to ‘lock’ your code
to a specific version too.

``` r
library(phslookups)

spd_23 <- get_spd(version = "2023_2")
localities_mar_24 <- get_hscp_locality(version = "20240308")
simd_dz_2016 <- get_simd_datazone(simd_version = "2016")
simd_pc_2016_2012 <- get_simd_postcode(postcode_version = "2016_1", simd_version = "2012")
```

Select only the necessary columns to reduce memory use and improve
speed.

``` r
library(phslookups)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

postcode_coords <- get_spd(col_select = c("pc7", "latitude", "longitude"))
#> ℹ Using the latest available version:
#> "Scottish_Postcode_Directory_2026_1".
#> If you require an older version or for reproducibility purposes
#> please specify the version argument accordingly.
locality_lookup <- get_hscp_locality(col_select = c("datazone2011", "hscp_locality"))
#> ℹ Using the latest available version:
#> "HSCP Localities_DZ11_Lookup_20240513".
#> If you require an older version or for reproducibility purposes
#> please specify the version argument accordingly.

simd_dz_2016 <- get_simd_datazone(simd_version = "2016")

get_simd_datazone(
  simd_version = "2016", 
  col_select = c("DataZone2011", contains("quintile"))
  )
#> # A tibble: 6,976 × 5
#>    DataZone2011 simd2016_sc_quintile simd2016_HB2014_quintile
#>    <chr>                       <int>                    <int>
#>  1 S01006506                       4                        3
#>  2 S01006507                       4                        3
#>  3 S01006508                       5                        5
#>  4 S01006509                       4                        4
#>  5 S01006510                       3                        2
#>  6 S01006511                       5                        5
#>  7 S01006512                       5                        4
#>  8 S01006513                       5                        4
#>  9 S01006514                       5                        5
#> 10 S01006515                       5                        5
#> # ℹ 6,966 more rows
#> # ℹ 2 more variables: simd2016_HSCP2016_quintile <int>,
#> #   simd2016_CA2011_quintile <int>
```
