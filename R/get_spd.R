#' Get the Scottish Postcode Directory
#'
#' @param version Default is "latest", otherwise supply a tag e.g. "2023_2"
#'
#' @return a [tibble][tibble::tibble-package] of the Scottish Postcode Directory
#' @export
#'
#' @examples
#' get_spd()
get_spd <- function(version = "latest") {
  # Postcode directory
  lookups_dir <- fs::path("/conf/linkage/output/lookups/Unicode")

  spd_dir <- fs::path(lookups_dir, "Geography/Scottish Postcode Directory")

  if (version == "latest") {
    spd_path <- find_latest_file(
      directory = spd_dir,
      regexp = "Scottish_Postcode_Directory_\\d{4}_[1-2]\\.parquet",
      selection_method = "file_name"
    )
  } else {
    spd_path <- fs::path(spd_dir, glue::glue("Scottish_Postcode_Directory_{version}.parquet"))
  }


  return(arrow::read_parquet(spd_path) %>% tibble::as_tibble())
}
