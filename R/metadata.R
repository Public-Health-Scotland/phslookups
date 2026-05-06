#' Function to access metadata
#'
#' @param data Dataset imported using one of the [phslookups] functions,
#' e.g. `get_spd()`.
#'
#' @returns
#' Metadata `tibble` associated with dataset.
#'
#' @examples
#' library(phslookups)
#' \dontrun{
#' spd <- get_spd()
#' metadata(spd)
#' }
#'
#' @export
metadata <- function(data) {
  if (!inherits(data, "tbl_df")) {
    cli::cli_abort(
      "{.arg data} must must be a tibble loaded using {.pkg phslookups}."
    )
  }

  # If already loaded, return immediately
  metadata <- attr(data, "metadata", exact = TRUE)
  if (!is.null(metadata)) {
    return(metadata)
  }

  ref <- attr(data, "metadata_ref", exact = TRUE)

  if (is.null(ref)) {
    cli::cli_abort("Metadata is not available for this data.")
  }

  if (rlang::is_false(ref$exists)) {
    cli::cli_abort(c(
      "x" = "{ref$type} metadata not available.",
      "i" = "Expected at {.path {ref$path}}"
    ))
  }

  metadata <- read_file(
    ref$path,
    col_select = 1:2,
    col_names = c("variable", "description"),
    skip = 1,
    col_types = readr::cols_only(
      variable = readr::col_character(),
      description = readr::col_character()
    )
  )

  inform_metadata_version(ref$version)

  # Attach metadata to the object
  set_metadata(data, metadata)

  metadata
}

set_metadata_ref <- function(data, path, type, version, exists) {
  attr(data, "metadata_ref") <- list(
    path = path,
    type = type,
    version = version,
    exists = exists
  )
  data
}

set_metadata <- function(data, metadata) {
  attr(data, "metadata") <- metadata
  data
}

inform_metadata_access <- function(metadata) {
  cli::cli_inform(c(
    i = "Metadata is available and can be accessed using {.fun metadata}."
  ))
}

inform_metadata_version <- function(version) {
  if (version != "latest") {
    cli::cli_warn(
      "Metadata corresponds to the latest version of the data
      and may not exactly match the data currently loaded."
    )
  }
}
