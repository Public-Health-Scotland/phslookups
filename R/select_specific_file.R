#' Select a specific file in format that exists in Archive
#'
#' @param directory The directory in which to search.
#' @param name Name of the file without version and extension.
#' @param version Version of the file.
#' @return the [fs::path()] to the file
#' @noRd
#' @keywords internal
select_specific_file <-
  function(directory, name, version) {
    name_ver_list <- paste0(name, version, c(".parquet", ".rds", ".csv"))
    path_ver_list <- fs::path(directory, "Archive", name_ver_list)
    path <- path_ver_list[fs::file_exists(path_ver_list)][1]
    return(path)
  }
