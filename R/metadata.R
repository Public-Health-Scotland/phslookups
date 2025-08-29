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

set_metadata <- function(data, metadata){
  attr(data, "metadata") <- metadata
  data
}

inform_metadata_access <- function(){
  cli::cli_inform(c("", "i" = "SPD metadata has been attached to the data and can be accessed via `metadata()`"))
}

inform_metadata_version <- function(version){
  if (version != "latest"){
    cli::cli_warn("Metadata is based on the latest version of the data and may not be relevant to the requested data")
  }
}
