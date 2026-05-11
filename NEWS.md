# phslookups 0.2.0

-   Fixed a typo in the lookup path that meant the package wouldn't work on Windows (RStudio Desktop).
-   Make the minimum required R version 4.1
-   Metadata is now available! Currently, this is only for the Scottish Postcode Directory (`get_spd()`), but we will bring it to the other lookups soon, too. You can see the metadata by using `metadata()` on the lookup object, for example:
```r
spd <- get_spd()
metadata(spd)
```

# phslookups 0.1.0

-   Initial version, which includes: [`get_hscp_locality()`](https://public-health-scotland.github.io/phslookups/reference/get_hscp_locality.html), [`get_simd_datazone()`](https://public-health-scotland.github.io/phslookups/reference/get_simd_datazone.html), [`get_simd_postcode()`](https://public-health-scotland.github.io/phslookups/reference/get_simd_postcode.html), and [`get_spd()`](https://public-health-scotland.github.io/phslookups/reference/get_spd.html)
