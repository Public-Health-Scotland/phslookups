#' Get the Scottish Postcode Directory
#'
#' @return a [tibble][tibble::tibble-package] of the
#' Scottish Postcode Directory
#' @export
#'
#' @examples
get_spd <- function() {
  # Postcode directory
  lookups_dir <- fs::path("/conf/linkage/output/lookups/Unicode")

  spd_dir <- fs::path(lookups_dir, "Geography/Scottish Postcode Directory")
  spd_path <- fs::path(spd_dir, "Scottish_Postcode_Directory_2020_2.rds")

  if (!fs::file_exists(spd_path)) {
    stop("Check the SPD path, likely there is a new version")
  } else {
    return(readr::read_rds(spd_path) %>% tibble::as_tibble())
  }
}
