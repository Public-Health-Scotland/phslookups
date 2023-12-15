## code to prepare `hscp_locality_variables` dataset goes here

hscp_locality_variables <- names(get_hscp_locality())

usethis::use_data(hscp_locality_variables, overwrite = TRUE)
