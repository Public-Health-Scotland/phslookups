## code to prepare `simd_postcode_variables` dataset goes here

simd_postcode_variables <- names(get_simd_postcode())

usethis::use_data(simd_postcode_variables, overwrite = TRUE)
