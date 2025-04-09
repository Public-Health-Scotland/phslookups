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
#'
#' @return the [fs::path()] to the file
#' @export
#'
#' @examples
#' \dontrun{
#' find_latest_file(
#'   directory = "/conf/linkage/output/lookups/Unicode",
#'   regexp = "Scottish_Postcode_Directory_.+?\\.rds"
#' )
#' }
find_latest_file <- function(directory,
                             regexp,
                             selection_method = "modification_date") {
  if (selection_method == "modification_date") {
    latest_file <- fs::dir_info(
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
      magrittr::extract(1L, )
  } else if (selection_method == "file_name") {
    latest_file <- fs::dir_info(
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
      magrittr::extract(1L, )
  }

  if (nrow(latest_file) == 1L) {
    cli::cli_alert_info(
      "Using the latest available version: {.val {fs::path_file(
       fs::path_ext_remove(latest_file$path))}}.
       If you require an older version specify an argument `version`.")
  } else {
    cli::cli_abort(
      "There was no file in {.path {directory}} that matched the
       regular expression {.val {regexp}}"
    )
  }

  file_path <- latest_file |>
    dplyr::pull(.data$path)

  return(file_path)
}
