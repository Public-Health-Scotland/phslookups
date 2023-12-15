#' Get the HSCP Locality lookup
#'
#' @param version Default is "latest", otherwise supply a date e.g. "20230804"
#'
#' @return a [tibble][tibble::tibble-package] of the HSCP localities lookup
#' @export
#'
#' @examples
#' get_hscp_locality()
get_hscp_locality <- function(version = "latest") {
  dir <- fs::path(get_lookups_dir(), "Geography", "HSCP Locality")

  if (version == "latest") {
    hscp_locality_path <- find_latest_file(
      directory = dir,
      regexp = "HSCP Localities_DZ11_Lookup_\\d{8}\\.rds",
      selection_method = "file_name"
    )
  } else {
    hscp_locality_path <- fs::path(
      dir,
      glue::glue("HSCP Localities_DZ11_Lookup_{date}.rds")
    )
  }

  return(read_file(hscp_locality_path))
}
