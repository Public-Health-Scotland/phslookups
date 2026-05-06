# Function to access metadata

Function to access metadata

## Usage

``` r
metadata(data)
```

## Arguments

- data:

  Dataset imported using one of the phslookups functions, e.g.
  [`get_spd()`](https://moohan.github.io/phslookups/reference/get_spd.md).

## Value

Metadata `tibble` associated with dataset.

## Examples

``` r
library(phslookups)
if (FALSE) { # \dontrun{
spd <- get_spd()
metadata(spd)
} # }
```
