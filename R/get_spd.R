#' Get Scottish Postcode Directory lookup
#'
#' Read a Scottish Postcode Directory (SPD) lookup file from cl-out into
#' a tibble.
#' @param version A string defining a version to read in. The default value
#'  is "latest" and the latest SPD file available on cl-out will be loaded.
#'  Alternatively you can supply a string defining a specific version that you
#'  would like to load. It should follow pattern "YYYY_1" or "YYYY_2",
#'  e.g. "2023_2". See details for further information.
#' @inheritParams readr::read_csv
#'
#' @details
#' SPD lookup files are sourced from the following folder
#' `\\stats\cl-out\lookups\Unicode\Geography\Scottish Postcode Directory`
#' and its `Archive` subfolder.
#' They are updated twice a year, which is denoted by the suffix of their
#' name: Scottish_Postcode_Directory_YYYY_X", where YYYY denotes a year and
#' X denotes release number for this year (X = 1 or X = 2). Please note that
#' the oldest available version is "2016_1".
#'
#' @return A [tibble][tibble::tibble-package] of the Scottish Postcode
#'  Directory lookup file or its selected columns.
#' @export
#'
#' @examples
#' get_spd()
#' get_spd(version = "2023_2", col_select = c("pc7", "latitude", "longitude"))
get_spd <- function(version = "latest", col_select = NULL) {
  dir <- fs::path(
    get_lookups_dir(), "Geography",
    "Scottish Postcode Directory"
  )

  metadata_dir <- fs::path(dir, "Metadata")

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

    spd_path <- find_specific_file(
      directory = dir,
      lookup_type = "SPD",
      version = version
    )
  }

  metadata <- readr::read_csv(fs::path(metadata_dir, "spd_metadata.csv")) %>%
    dplyr::select(1:2) %>%
    stats::setNames(c("variable", "description"))

  inform_metadata_access()

  inform_metadata_version(version)

  cli::cat_line("\n--- Metadata ---\n", col = "blue")
  cli::cat_print(metadata)
  cli::cat_line("")

  spd <- read_file(
    spd_path,
    col_select = {{ col_select }}
  )
  spd <- set_metadata(spd, metadata)
  return(spd)
}
