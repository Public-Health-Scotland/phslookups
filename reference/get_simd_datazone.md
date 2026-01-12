# Get Data Zone-SIMD lookup

Read a Data Zone-Scottish Index of Multiple Deprivation (SIMD) lookup
file from cl-out into a tibble.

## Usage

``` r
get_simd_datazone(simd_version = "latest", col_select = NULL)
```

## Arguments

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
of the SIMD DataZone lookup

## Examples

``` r
get_simd_datazone()
#> Error: [ENOENT] Failed to search directory '/conf/linkage/output/lookups/Unicode/Deprivation': no such file or directory
get_simd_datazone(simd_version = "2016")
#> Error: [ENOENT] Failed to search directory '/conf/linkage/output/lookups/Unicode/Deprivation': no such file or directory
get_simd_datazone(
  simd_version = "2016",
  col_select = c("DataZone2011", "simd2016rank")
)
#> Error: [ENOENT] Failed to search directory '/conf/linkage/output/lookups/Unicode/Deprivation': no such file or directory
```
