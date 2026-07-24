# Get National Reference File

Read a National Reference File.

## Usage

``` r
get_national_ref_file(filename, col_select = NULL)
```

## Arguments

- filename:

  A character string giving the file name to read. Any file extension
  supplied will be ignored and matching is case-insensitive.

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

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
of the requested National Reference File.

## Examples

``` r
get_national_ref_file("gpprac")
#> Error: [ENOENT] Failed to search directory '/conf/linkage/output/lookups/Unicode/National Reference Files': no such file or directory
get_national_ref_file("GPprac.csv", col_select = c("praccode", "postcode"))
#> Error: [ENOENT] Failed to search directory '/conf/linkage/output/lookups/Unicode/National Reference Files': no such file or directory
get_national_ref_file("specialty_groupings")
#> Error: [ENOENT] Failed to search directory '/conf/linkage/output/lookups/Unicode/National Reference Files': no such file or directory
```
