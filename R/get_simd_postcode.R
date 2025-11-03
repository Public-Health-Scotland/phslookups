#' Get Postcode-SIMD lookup
#'
#' Read a Postcode-Scottish Index of Multiple Deprivation (SIMD) lookup file
#' from cl-out into a tibble.
#' @param postcode_version A string defining a postcode version. The default
#'  value is "latest". Alternatively you can supply a string defining
#'  a specific version that you would like to load. It should follow pattern
#'  "YYYY_1" or "YYYY_2", e.g. "2023_2".
#' @param simd_version A string defining a SIMD version. The default value
#'  is "latest". Alternatively you can supply a string defining a specific
#'  version. It should follow pattern "YYYY" or "YYYYv2", e.g. "2020v2".
#' @inheritParams readr::read_csv
#'
#' @return a [tibble][tibble::tibble-package] of the SIMD Postcode lookup
#' @export
#'
#' @examples
#' get_simd_postcode()
#' get_simd_postcode(postcode_version = "2016_1", simd_version = "2012")
#'
#' library(dplyr)
#' get_simd_postcode(
#'   postcode_version = "2016_1",
#'   simd_version = "2012",
#'   col_select = c("pc7", starts_with("simd"))
#' )
get_simd_postcode <- function(
  postcode_version = "latest",
  simd_version = "latest",
  col_select = NULL
) {
  dir <- fs::path(get_lookups_dir(), "Deprivation")

  if (postcode_version == "latest" && simd_version == "latest") {
    regexp <- paste0(
      "postcode_",
      ifelse(postcode_version == "latest", "\\d{4}_[1-2]", postcode_version),
      "_simd",
      ifelse(simd_version == "latest", "\\d{4}(:?v[1-2])?", simd_version),
      "\\.parquet"
    )

    simd_postcode_path <- find_latest_file(
      directory = dir,
      regexp = regexp,
      selection_method = "file_name"
    )
  } else if (postcode_version != "latest" && simd_version != "latest") {
    valid_postcode_version <- stringr::str_detect(
      postcode_version,
      "\\d{4}_[1-2]"
    )
    valid_simd_version <- stringr::str_detect(
      simd_version,
      "^20[0-9]{2}(:?v2)?$"
    )

    if (!valid_postcode_version || !valid_simd_version) {
      cli::cli_abort(c(
        "x" = "Invalid version specification, Postcode:
        {.val {postcode_version}}, SIMD: {.val {simd_version}}",
        "i" = "Postcode should be follow the pattern YYYY_1 or YYYY_2",
        "i" = "SIMD should follow the pattern YYYY or YYYYv2"
      ))
    }

    simd_postcode_path <- find_specific_file(
      directory = dir,
      lookup_type = "SIMD Postcode",
      version = list(
        postcode_version = postcode_version,
        simd_version = simd_version
      )
    )
  } else {
    # Case when one of the versions is 'latest' but the other isn't
    cli::cli_abort(c(
      "x" = "When using a version other than {.val latest} both
      {.arg postcode_version} and {.arg simd_version} must be specified."
    ))
  }

  return(read_file(
    simd_postcode_path,
    col_select = {{ col_select }}
  ))
}
