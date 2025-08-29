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
metadata <- function(data){
  out <- attr(data, "metadata")

  if (is.null(out)){
    cli::cli_abort("Metadata could not be found, please check")
  }

  out
}
