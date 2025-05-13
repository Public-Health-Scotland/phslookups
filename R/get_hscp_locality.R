#' Get the HSCP Locality lookup
#'
#' @param version Default is "latest", otherwise supply a date e.g. "20230804"
#' @inheritParams readr::read_csv
#'
#' @return a [tibble][tibble::tibble-package] of the HSCP localities lookup
#' @export
#'
#' @examples
#' get_hscp_locality()
get_hscp_locality <- function(version = "latest", col_select = NULL) {
  dir <- fs::path(get_lookups_dir(), "Geography", "HSCP Locality")

  # If col_select is specified use CSV otherwise use RDS
  ext <- ifelse(rlang::quo_is_null(rlang::enquo(col_select)), "rds", "csv")

  if (version == "latest") {
    hscp_locality_path <- find_latest_file(
      directory = dir,
      regexp = paste0("HSCP Localities_DZ11_Lookup_\\d{8}\\.", ext),
      selection_method = "file_name"
    )
  } else {
    hscp_locality_path <- fs::path(
      dir,
      glue::glue("HSCP Localities_DZ11_Lookup_{date}.{ext}")
    )
  }

  return(read_file(hscp_locality_path, col_select = {{ col_select }}))
}
