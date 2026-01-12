# Get Scottish Postcode Directory lookup

Read a Scottish Postcode Directory (SPD) lookup file from cl-out into a
tibble.

## Usage

``` r
get_spd(version = "latest", col_select = NULL)
```

## Arguments

- version:

  A string defining a version to read in. The default value is "latest"
  and the latest SPD file available on cl-out will be loaded.
  Alternatively you can supply a string defining a specific version that
  you would like to load. It should follow pattern "YYYY_1" or "YYYY_2",
  e.g. "2023_2". See details for further information.

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
of the Scottish Postcode Directory lookup file or its selected columns.

## Details

SPD lookup files are sourced from the following folder
`\\stats\cl-out\lookups\Unicode\Geography\Scottish Postcode Directory`
and its `Archive` subfolder. They are updated twice a year, which is
denoted by the suffix of their name:
Scottish_Postcode_Directory_YYYY_X", where YYYY denotes a year and X
denotes release number for this year (X = 1 or X = 2). Please note that
the oldest available version is "2016_1".

## Examples

``` r
get_spd()
#> Error: [ENOENT] Failed to search directory '/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory': no such file or directory
get_spd(version = "2023_2", col_select = c("pc7", "latitude", "longitude"))
#> Error: ✖ "SPD" version "2023_2" is NOT available
#> ℹ Contact phs.geography@phs.scot
```
