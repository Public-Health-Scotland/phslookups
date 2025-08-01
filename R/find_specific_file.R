#' Find a specific version of a lookup file
#'
#' @description
#' This function retrieves the file path for a specific version of a lookup
#' file based on the provided directory and lookup type. It validates the
#' existence of the file and throws an error if no matching file is found.
#' For lookup types requiring multiple versions (SIMD Postcode),
#' `version` must be a named list or vector with the
#' appropriate names (e.g., `simd_version` and `postcode_version`).
#'
#' @param directory The base directory where lookup files are stored.
#' The function searches here and this directory's "Archive" subdirectory.
#' @param lookup_type A string specifying the type of lookup file to find.
#' Supported values include `"SPD"`, `"HSCP Locality"`, and `"SIMD Postcode"`.
#' @param version A string defining the version to locate, or a
#' named list/vector for lookup types requiring multiple versions.
#' For example:
#'   - `"YYYY_1"` for `lookup_type = "SPD"`
#'   - `"YYYYMMDD"` for `lookup_type = "HSCP Locality"`
#'   - `list(postcode_version = "2023_2", simd_version = "2020v2")` for
#'     `lookup_type = "SIMD Postcode"`
#'
#' @return The [fs::path()] of the file if found.
#' @keywords internal
#' @noRd
find_specific_file <- function(directory, lookup_type, version) {
  # Determine the file prefix and version handling based on the lookup_type
  if (length(version) == 1) {
    # Handle single-version cases
    file_prefix <- dplyr::case_match(
      lookup_type,
      "SPD" ~ paste0("Scottish_Postcode_Directory_", version),
      "HSCP Locality" ~ paste0("HSCP Localities_DZ11_Lookup_", version),
      .default = NA_character_
    )
  } else {
    # Handle multi-version cases
    file_prefix <- dplyr::case_match(
      lookup_type,
      "SIMD Postcode" ~
        paste0(
          "postcode_",
          version[["postcode_version"]],
          "_simd",
          version[["simd_version"]]
        ),
      .default = NA_character_
    )
  }

  if (is.na(file_prefix)) {
    cli::cli_abort("Unsupported lookup_type: {.val {lookup_type}}")
  }

  # Generate possible file names and paths
  name_ver_list <- paste0(
    file_prefix,
    c(".parquet", ".rds", ".csv")
  )
  path_ver_list <- c(
    fs::path(directory, name_ver_list),
    fs::path(directory, "Archive", name_ver_list)
  )

  # Find the first valid file that exists
  file_path <- path_ver_list[fs::file_exists(path_ver_list)][1]

  # Handle case where no matching file is found
  if (is.na(file_path)) {
    cli::cli_abort(
      c(
        "x" = "{.val {lookup_type}} version {.val {version}} is NOT available",
        "i" = "Contact phs.geography@phs.scot"
      ),
      call = NULL,
      rlang_backtrace_on_error = "none"
    )
  }

  return(file_path)
}
