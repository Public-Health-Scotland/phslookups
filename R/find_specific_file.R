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
  file_prefix <- switch(lookup_type,
    SPD = {
      if (!rlang::is_string(version)) {
        cli::cli_abort(
          "{.arg version} must be a single string for the SPD lookup."
        )
      }

      paste0("Scottish_Postcode_Directory_", version)
    },
    "HSCP Locality" = {
      if (!rlang::is_string(version)) {
        cli::cli_abort(
          paste0(
            "{.arg version} must be a single string for the ",
            "HSCP Locality lookup."
          )
        )
      }

      paste0("HSCP Localities_DZ11_Lookup_", version)
    },
    "SIMD Postcode" = {
      required_versions <- c(
        "postcode_version",
        "simd_version"
      )

      if (
        is.null(names(version)) ||
          !all(required_versions %in% names(version))
      ) {
        cli::cli_abort(
          paste0(
            "{.arg version} must contain both {.field postcode_version} ",
            "and {.field simd_version}."
          )
        )
      }

      paste0(
        "postcode_",
        version[["postcode_version"]],
        "_simd",
        version[["simd_version"]]
      )
    },
    NULL
  )

  if (is.null(file_prefix)) {
    cli::cli_abort(
      "Unsupported {.arg lookup_type}: {.val {lookup_type}}."
    )
  }

  file_path <- find_preferred_version(
    directory = c(directory, fs::path(directory, "Archive")),
    file_stem = file_prefix
  )

  # Handle case where no matching file is found
  if (is.na(file_path)) {
    version_label <- if (lookup_type == "SIMD Postcode") {
      paste(
        version[["postcode_version"]],
        version[["simd_version"]],
        sep = " / "
      )
    } else {
      version
    }

    cli::cli_abort(
      c(
        x = "{.val {lookup_type}} version {.val {version_label}} is NOT available",
        i = "Contact phs.geography@phs.scot"
      ),
      call = NULL,
      rlang_backtrace_on_error = "none"
    )
  }

  file_path
}
