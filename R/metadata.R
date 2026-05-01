#' Function to access metadata
#'
#' @param data Dataset imported via e.g. `get_spd()`.
#'
#' @returns
#' Metadata `tibble` associated with dataset.
#'
#' @examples
#' library(phslookups)
#'
#' \dontrun{
#' spd <- get_spd()
#'
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

  out <- attr(data, "metadata")

  if (is.null(out)) {
    cli::cli_abort("Metadata could not be found, please check")
  }

  out
}

set_metadata <- function(data, metadata) {
  attr(data, "metadata") <- metadata
  data
}

inform_metadata_access <- function(metadata) {
  meta_print <- utils::capture.output(
    print(metadata, n = 5)
  )

  cli::cli_inform(c(
    i = "Metadata has been attached and can be accessed using {.fun metadata}.",
    cli::col_blue("------ Metadata -----"),
    meta_print,
    cli::col_blue("------")
  ))
}

inform_metadata_version <- function(version) {
  if (version != "latest") {
    cli::cli_warn(
      "Metadata is correct for the latest version of the data,
      and may not be relevant to the version you have loaded."
    )
  }
}
