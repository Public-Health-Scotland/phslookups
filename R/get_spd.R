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
    col_select = all_of(names(.metadata$spd))) {

  spd_path <- get_spd_path(version, quiet = FALSE)

  return(read_file(
    spd_path,
    col_select = {{ col_select }}
  ))
}
