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

    name_ver_list <- paste0("Scottish_Postcode_Directory_", version,
                       c(".parquet", ".rds", ".csv"))
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
