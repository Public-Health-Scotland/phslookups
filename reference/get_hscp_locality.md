# Get HSCP Locality lookup

Read a Health and Social Care Partnership (HSCP) Locality lookup file
from cl-out into a tibble.

## Usage

``` r
get_hscp_locality(version = "latest", col_select = NULL)
```

## Arguments

- version:

  A string defining a version to read in. The default value is "latest",
  otherwise supply a date (file name suffix), e.g. "20230804".

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
of the HSCP localities lookup

## Examples

``` r
get_hscp_locality()
#> Error: [ENOENT] Failed to search directory '/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality': no such file or directory
get_hscp_locality(version = "20240308")
#> Error: ✖ "HSCP Locality" version "20240308" is NOT available
#> ℹ Contact phs.geography@phs.scot
get_hscp_locality(col_select = c("datazone2011", "hscp_locality"))
#> Error: [ENOENT] Failed to search directory '/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality': no such file or directory
```
