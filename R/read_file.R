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

  if ((!rlang::quo_is_null(rlang::enquo(col_select)) && ext != "parquet") {
    cli::cli_abort(c(
      "x" = "{.arg col_select} and/or {.arg as_data_frame} must only be used
        when reading a {.field .parquet} file."
    ))
  }

  data <- switch(ext,
    "rds" = readr::read_rds(file = path),
    "csv" = readr::read_csv(file = path, ..., show_col_types = FALSE),
    "parquet" = tibble::as_tibble(arrow::read_parquet(
      file = path,
      col_select = {{ col_select }},
      ...
    ))
  )

  return(data)
}
