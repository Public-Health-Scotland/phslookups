#' Get the Scottish Postcode Directory
#'
#' @param version Default is "latest", otherwise supply a tag e.g. "2023_2"
#' @inheritParams arrow::read_parquet
#'
#' @return a [tibble][tibble::tibble-package] of the Scottish Postcode Directory
#' @export
#'
#' @examples
#' get_spd()
#' get_spd(col_select = c("pc7", "latitude", "longitude"))
get_spd <- function(
    version = "latest",
    col_select = NULL) {
  dir <- fs::path(get_lookups_dir(), "Geography", "Scottish Postcode Directory")

  if (version == "latest") {
    spd_path <- find_latest_file(
      directory = dir,
      regexp = "Scottish_Postcode_Directory_\\d{4}_[1-2]\\.parquet",
      selection_method = "file_name"
    )
  } else {
    if (!stringr::str_detect(version, "^20\\d{2}_[1-2]$")) {
      cli::cli_abort(c(
        "x" = "Invalid version name: {.val {version}}",
        "i" = "It should follow pattern YYYY_1 or YYYY_2",
        call = NULL
      ))
    }
    spd_path <- fs::path(
      dir, "Archive",
      glue::glue("Scottish_Postcode_Directory_{version}.parquet")
    )
  }

  return(read_file(
    spd_path,
    col_select = {{ col_select }}
  ))
}
