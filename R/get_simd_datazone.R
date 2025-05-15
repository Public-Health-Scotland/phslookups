#' Get a SIMD DataZone Lookup
#'
#' @param datazone_version Default is "latest", otherwise supply a year
#' e.g. "2011"
#' @inheritParams get_simd_postcode
#'
#' @return a [tibble][tibble::tibble-package] of the SIMD DataZone lookup
#' @export
#'
#' @examples
#' get_simd_datazone()
#' get_simd_datazone(datazone_version = "2011", simd_version = "2016")
#' get_simd_datazone(
#'   datazone_version = "2011",
#'   simd_version = "2016",
#'   col_select = c("DataZone2011", "simd2016rank")
#' )
get_simd_datazone <- function(
    datazone_version = "latest",
    simd_version = "latest",
    col_select = NULL) {
  dir <- fs::path(get_lookups_dir(), "Deprivation")

  if (datazone_version == "latest" && simd_version == "latest") {
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
  } else if (datazone_version != "latest" && simd_version != "latest") {
    valid_datazone_version <- stringr::str_detect(datazone_version, "^20[0-9]{2}$")
    valid_simd_version <- stringr::str_detect(datazone_version, "^20[0-9]{2}(:?v2)?$")

    if (!valid_datazone_version || !valid_simd_version) {
      cli::cli_abort(c(
        "x" = "Invalid version specification, DataZone: {.val {datazone_version}}, SIMD: {.val {simd_version}}",
        "i" = "DataZone should be a 4-digit Year (YYYY)",
        "i" = "SIMD should follow the pattern YYYY or YYYYv2"
      ))
    }

    simd_datazone_path <- find_specific_file(
      directory = dir,
      lookup_type = "SIMD DataZone",
      version = list(datazone_version = datazone_version, simd_version = simd_version)
    )
  } else {
    # Case when one of the versions is 'latest' but the other isn't
    cli::cli_abort(c(
      "x" = "When using a version other than {.val latest} both {.arg datazone_version} and {.arg simd_version} must be specified."
    ))
  }

  return(read_file(simd_datazone_path, col_select = {{ col_select }}))
}
