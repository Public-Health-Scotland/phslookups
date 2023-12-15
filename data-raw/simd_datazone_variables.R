## code to prepare `simd_datazone_variables` dataset goes here

simd_datazone_variables <- names(get_simd_datazone())

usethis::use_data(simd_datazone_variables, overwrite = TRUE)
