## code to prepare `spd_variables` dataset goes here

spd_variables <- names(get_spd())

usethis::use_data(spd_variables, overwrite = TRUE)
