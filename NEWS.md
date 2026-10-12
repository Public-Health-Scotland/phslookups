# phslookups (development version)

- `get_national_ref_file()` is a new function for reading lookups in the 'National Reference Files'. Simply provide the name of the file you need e.g. `get_national_ref_file("gpprac")`.

# phslookups 0.2.0

-   Fixed a typo in the lookup path that meant the package wouldn't work on Windows (RStudio Desktop).
-   Make the minimum required R version 4.1
-   Overhaul the README; fixed out of date information, and added more detail and examples.
-   Metadata is now available! Currently, this is only for the Scottish Postcode Directory (`get_spd()`), but we will bring it to the other lookups soon, too. You can see the metadata by using `metadata()` on the lookup object, for example:
```r
spd <- get_spd()
metadata(spd)
```

# phslookups 0.1.0

-   Initial version, which includes: [`get_hscp_locality()`](https://public-health-scotland.github.io/phslookups/reference/get_hscp_locality.html), [`get_simd_datazone()`](https://public-health-scotland.github.io/phslookups/reference/get_simd_datazone.html), [`get_simd_postcode()`](https://public-health-scotland.github.io/phslookups/reference/get_simd_postcode.html), and [`get_spd()`](https://public-health-scotland.github.io/phslookups/reference/get_spd.html)
