# Get Postcode-SIMD lookup

Read a Postcode-Scottish Index of Multiple Deprivation (SIMD) lookup
file from cl-out into a tibble.

## Usage

``` r
get_simd_postcode(
  postcode_version = "latest",
  simd_version = "latest",
  col_select = NULL
)
```

## Arguments

- postcode_version:

  A string defining a postcode version. The default value is "latest".
  Alternatively you can supply a string defining a specific version that
  you would like to load. It should follow pattern "YYYY_1" or "YYYY_2",
  e.g. "2023_2".

- simd_version:

  A string defining a SIMD version. The default value is "latest".
  Alternatively you can supply a string defining a specific version. It
  should follow pattern "YYYY" or "YYYYv2", e.g. "2020v2".

- col_select:

  Columns to include in the results. You can use the same mini-language
  as
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  to refer to the columns by name. Use
  [`c()`](https://rdrr.io/r/base/c.html) to use more than one selection
  expression. Although this usage is less common, `col_select` also
  accepts a numeric column index. See
  [`?tidyselect::language`](https://tidyselect.r-lib.org/reference/language.html)
  for full details on the selection language.

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
of the SIMD Postcode lookup

## Examples

``` r
get_simd_postcode()
#> Error: [ENOENT] Failed to search directory '/conf/linkage/output/lookups/Unicode/Deprivation': no such file or directory
get_simd_postcode(postcode_version = "2016_1", simd_version = "2012")
#> Error: ✖ "SIMD Postcode" version 2016_1 and 2012 is NOT available
#> ℹ Contact phs.geography@phs.scot

library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
get_simd_postcode(
  postcode_version = "2016_1",
  simd_version = "2012",
  col_select = c("pc7", starts_with("simd"))
)
#> Error: ✖ "SIMD Postcode" version 2016_1 and 2012 is NOT available
#> ℹ Contact phs.geography@phs.scot
```
