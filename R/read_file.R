#' Read a file
#'
#' @description Read a file, the function chosen to read the file is dependent
#' on the file path.
#'  * `.rds` uses [readr::read_rds()].
#'  * `.csv` use [readr::read_csv()].
#'  * `.parquet` uses [arrow::read_parquet()].
#'
#' @param path The file path to be read
#' @inheritParams arrow::read_parquet
#' @param ... Addition arguments passed to the relevant function.
#'
#' @return the data a [tibble][tibble::tibble-package]
#' @noRd
#' @keywords internal
read_file <- function(path, col_select = NULL, ...) {
  valid_extensions <- c(
    "rds",
    "csv",
    "parquet"
  )
  ext <- fs::path_ext(path)

  if (!(ext %in% valid_extensions)) {
    cli::cli_abort(c(
      "x" = "Invalid extension: {.val {ext}}",
      "i" = "{.fun read_file} supports
                     {.val {valid_extensions}}"
    ))
  }

  if (!(file.exists(path))) {
    cli::cli_abort(
      c(
        "x" = "File {.val {fs::path_file(fs::path_ext_remove(path))}}
             is NOT available",
        "i" = "Contact {.email phs.geography@phs.scot}"
      ),
      call = NULL, rlang_backtrace_on_error = "none"
    )
  }

  data <- switch(ext,
    "rds" = tibble::as_tibble(readr::read_rds(file = path)),
    "csv" = readr::read_csv(
      file = path, guess_max = 50000, ...,
      show_col_types = FALSE
    ),
    "parquet" = tibble::as_tibble(arrow::read_parquet(
      file = path,
      col_select = {{ col_select }},
      ...
    ))
  )

  # If col_select was supplied keep only those variables
  # This may sometimes be redundant, but it offers a final guarantee.
  if (!rlang::quo_is_null(rlang::enquo(col_select))) {
    data <- dplyr::select(data, {{ col_select }})
  }

  return(data)
}
