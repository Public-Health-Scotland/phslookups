#' Get Data Zone-SIMD lookup
#'
#' Read a Data Zone-Scottish Index of Multiple Deprivation (SIMD) lookup file
#' from cl-out into a tibble.
#' @inheritParams get_simd_postcode
#'
#' @return a [tibble][tibble::tibble-package] of the SIMD DataZone lookup
#' @export
#'
#' @examples
#' get_simd_datazone()
#' get_simd_datazone(simd_version = "2016")
#' get_simd_datazone(
#'   simd_version = "2016",
#'   col_select = c("DataZone2011", "simd2016rank")
#' )
get_simd_datazone <- function(simd_version = "latest", col_select = NULL) {
  dir <- fs::path(get_lookups_dir(), "Deprivation")

  if (simd_version == "latest") {
    regexp <- paste0(
      "DataZone",
      "\\d{4}",
      "_simd",
      "\\d{4}(:?v2)?",
      "\\.rds"
    )
  } else {
    valid_simd_version <- stringr::str_detect(
      string = simd_version,
      pattern = "^20[0-9]{2}(:?v2)?$"
    )

    if (!valid_simd_version) {
      cli::cli_abort(c(
        "x" = "Invalid version specification of SIMD: {.val {simd_version}}",
        "i" = "SIMD should follow the pattern YYYY or YYYYv2"
      ))
    }

    if (!(simd_version %in% c("2004", "2006", "2009v2", "2012",
                              "2016", "2020v2"))) {
      cli::cli_abort(c(
        "x" = "SIMD version {.val {simd_version}} does NOT exit.",
        "i" = "Note that \"2009\" and \"2020\" versions have been corrected and
               replaced with versions \"2009v2\" and \"2020v2\" respectively."
      ))
    }

    regexp <- paste0(
      "DataZone",
      "\\d{4}",
      "_simd",
      simd_version,
      "\\.rds"
    )
  }

  simd_datazone_path <- find_latest_file(
    directory = dir,
    regexp = regexp,
    selection_method = "file_name",
    quiet = simd_version != "latest"
  )

  return(read_file(simd_datazone_path, col_select = {{ col_select }}))
}
