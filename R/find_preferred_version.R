#' Find a file, preferring file extensions in order
#'
#' @param directory Directory to search.
#' @param file_stem File name without extension.
#' @param extensions File extensions to try, in preference order.
#' @param include_archive Should the "Archive" subdirectory also be searched?
#'
#' @return A file path if found, otherwise `NA`.
#' @keywords internal
#' @noRd
find_preferred_version <- function(
  directory,
  file_stem,
  extensions = c("parquet", "rds", "csv")
) {
  name_list <- paste0(
    file_stem,
    ".",
    extensions
  )

  path_combs <- expand.grid(
    directories = directory,
    file_names = name_list,
    stringsAsFactors = FALSE
  )

  path_list <- fs::path(path_combs$directories, path_combs$file_names)

  path_list[fs::file_exists(path_list)][1L]
}
