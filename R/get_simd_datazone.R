#' Get a SIMD DataZone Lookup
#'
#' @param datazone_version Default is "latest", otherwise supply a year
#' e.g. "2011"
#' @param simd_version Default is "latest", otherwise supply a version
#' e.g. "2020v2"
#'
#' @return a [tibble][tibble::tibble-package] of the SIMD DataZone lookup
#' @export
#'
#' @examples
#' get_simd_datazone()
get_simd_datazone <- function(
    datazone_version = "latest",
    simd_version = "latest") {
  dir <- fs::path(get_lookups_dir(), "Deprivation")

  if (datazone_version != "latest" && simd_version != "latest") {
    simd_datazone_path <- fs::path(
      dir,
      glue::glue("DataZone_{datazone_version}_simd{simd_version}.rds")
    )
  } else {
    regexp <- paste0(
      "DataZone",
      ifelse(datazone_version == "latest", "\\d{4}", datazone_version),
      "_simd",
      ifelse(simd_version == "latest", "\\d{4}(:?v[1-2])?", simd_version),
      "\\.rds"
    )

    simd_datazone_path <- find_latest_file(
      directory = dir,
      regexp = regexp,
      selection_method = "file_name"
    )
  }

  return(read_file(simd_datazone_path))
}
