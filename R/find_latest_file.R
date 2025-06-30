#' Find the latest version of a file
#'
#' @description
#' This will return the latest created file matching
#' the criteria. It uses [fs::dir_info()] to
#' find the files then picks the one with the latest
#' `birthtime`.
#'
#' @param directory The directory in which to search.
#' @param regexp a
#' [regular expression](https://www.regular-expressions.info/quickstart.html)
#' passed to [fs::dir_info()] to search for the file.
#' @param selection_method Valid arguments are "modification_date"
#' (the default) or "file_name".
#' @param quiet (default: FALSE) Used to suppress message output
#'
#' @return the [fs::path()] to the file
#' @examples
#' \dontrun{
#' find_latest_file(
#'   directory = "/conf/linkage/output/lookups/Unicode",
#'   regexp = "Scottish_Postcode_Directory_.+?\\.rds"
#' )
#' }
#' @noRd
#' @keywords internal
find_latest_file <- function(
    directory,
    regexp,
    selection_method = "modification_date",
    quiet = FALSE) {
  if (selection_method == "modification_date") {
    latest_file_options <- fs::dir_info(
      path = directory,
      type = "file",
      regexp = regexp,
      recurse = TRUE
    ) |>
      dplyr::arrange(
        dplyr::desc(.data$birth_time),
        dplyr::desc(.data$modification_time),
        dplyr::desc(.data$path)
      ) |>
      dplyr::pull(.data$path)
  } else if (selection_method == "file_name") {
    latest_file_options <- fs::dir_info(
      path = directory,
      type = "file",
      regexp = regexp,
      recurse = FALSE
    ) |>
      dplyr::arrange(
        dplyr::desc(.data$path),
        dplyr::desc(.data$birth_time),
        dplyr::desc(.data$modification_time)
      ) |>
      dplyr::pull(.data$path)
  }

  if (length(latest_file_options) >= 1) {
    file_path <- latest_file_options |>
      dplyr::first()

    if (!quiet) {
      cli::cli_alert_info(
        "Using the latest available version: {.val {fs::path_file(fs::path_ext_remove(file_path))}}.
       If you require an older version or for reproducibility purposes
       please specify the version argument accordingly."
      )
    }

    return(file_path)
  } else {
    cli::cli_abort(
      "There was no file in {.path {directory}} that matched the
       regular expression {.val {regexp}}"
    )
  }
}
