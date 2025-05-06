#' Get the Scottish Postcode Directory
#'
#' Read a Scottish Postcode Directory (SPD) lookup file into a tibble.
#' @param version A string defining a version to read in. The default value
#'  is "latest" and the latest SPD file available on cl-out will be loaded.
#'  Alternatively you can supply a tag, e.g. "2023_2", to load a specific file.
#' @param col_select A character vector of column names to keep, as in
#'  the "select" argument to data.table::fread(), or a tidy selection
#'  specification of columns, as used in dplyr::select().
#'
#' @return A [tibble][tibble::tibble-package] of the Scottish Postcode
#'  Directory lookup file or its selected columns.
#' @export
#'
#' @examples
#' get_spd()
#' get_spd(version = "2023_2", col_select = c("pc7", "latitude", "longitude"))
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

    name_ver_list <- paste0(
      "Scottish_Postcode_Directory_", version,
      c(".parquet", ".rds", ".csv")
    )
    path_ver_list <- fs::path(dir, "Archive", name_ver_list)
    spd_path <- path_ver_list[file.exists(path_ver_list)][1]
  }

  if (is.na(spd_path)) {
    cli::cli_abort(
      c(
        "x" = "SPD version {.val {version}} is NOT available",
        "i" = "Contact phs.geography@phs.scot"
      ),
      call = NULL, rlang_backtrace_on_error = "none"
    )
  }

  return(read_file(
    spd_path,
    col_select = {{ col_select }}
  ))
}
