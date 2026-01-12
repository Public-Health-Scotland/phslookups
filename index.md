# phslookups

The goal of phslookups is to provide helper functions to easily access
and use lookups which have been provided for analysis.

## Installation

You can install the latest version of phslookups from
[GitHub](https://github.com) with:

``` r
remotes::install_github("Public-Health-Scotland/phslookups")
```

It is not yet available from the PHS Posit Package Manager, it will
never be available on CRAN as it is intended to be an internal only
package.

## Example

Load the Scottish Postcode Directory

``` r
library(phslookups)

spd <- get_spd()
#> ℹ Using "Scottish_Postcode_Directory_2024_1.parquet".
```
