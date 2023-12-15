#' Get a SIMD Postcode Lookup
#'
#' @param postcode_version Default is "latest", otherwise supply a tag
#' e.g. "2023_2"
#' @param simd_version Default is "latest", otherwise supply a version
#' e.g. "2020v2"
#' @inheritParams arrow::read_parquet
#'
#' @return a [tibble][tibble::tibble-package] of the SIMD Postcode lookup
#' @export
#'
#' @examples
#' get_simd_postcode()
get_simd_postcode <- function(
    postcode_version = "latest",
    simd_version = "latest",
    col_select = NULL,
    as_data_frame = TRUE) {
  dir <- fs::path(get_lookups_dir(), "Deprivation")

  if (postcode_version != "latest" && simd_version != "latest") {
    simd_postcode_path <- fs::path(
      dir,
      glue::glue("postcode_{postcode_version}_simd{simd_version}.parquet")
    )
  } else {
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
  }

  return(read_file(
    simd_postcode_path,
    col_select = col_select,
    as_data_frame = as_data_frame
  ))
}
