#' Get National Reference File
#'
#' Read a National Reference File.
#'
#' @param filename A character string giving the file name to read. Any file
#' extension supplied will be ignored and matching is case-insensitive.
#' @inheritParams readr::read_csv
#'
#' @return A [tibble][tibble::tibble-package] of the requested National
#' Reference File.
#' @export
#'
#' @examples
#' get_national_ref_file("gpprac")
#' get_national_ref_file("GPprac.csv", col_select = c("praccode", "postcode"))
#' get_national_ref_file("specialty_groupings")
get_national_ref_file <- function(filename, col_select = NULL) {
  if (!rlang::is_string(filename) || !nzchar(filename)) {
    cli::cli_abort(
      "{.arg filename} must be a single non-empty character string."
    )
  }

  national_ref_dir <- fs::path(
    get_lookups_dir(),
    "National Reference Files"
  )

  raw_filename <- filename |>
    fs::path_file() |>
    fs::path_ext_remove()

  available_files <- fs::dir_ls(
    path = national_ref_dir,
    type = "file",
    recurse = FALSE
  )

  available_file_stems <- available_files |>
    fs::path_file() |>
    fs::path_ext_remove()

  matched_files <- available_files[
    tolower(available_file_stems) == tolower(raw_filename)
  ]

  if (length(matched_files) == 0L) {
    not_found_msg <- c(
      x = "National Reference File {.val {filename}} was not found.",
      i = "Matching is case-insensitive and ignores the file extension."
    )

    distances <- utils::adist(
      raw_filename,
      available_file_stems,
      ignore.case = TRUE
    ) |>
      drop()

    close_match_indices <- which(distances <= 2L)

    if (length(close_match_indices) > 0L) {
      close_match_indices <- close_match_indices[
        order(distances[close_match_indices])
      ]

      cli::cli_abort(
        c(
          not_found_msg,
          i = "Did you mean: {.or {.val {available_file_stems[close_match_indices]}}}?"
        )
      )
    }

    cli::cli_abort(not_found_msg)
  }

  # If there are multiple matches, pick the preferred file type.
  if (length(matched_files) > 1L) {
    matched_files <- find_preferred_version(
      national_ref_dir,
      raw_filename
    )
  }

  read_file(matched_files, col_select = {{ col_select }})
}
