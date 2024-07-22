#' Check access to lookups
#'
#' @description
#' `have_access()` provides a convenient way to check that you have the
#' correct access to the necessary folders.
#'
#' @returns
#' `[logical(1)]` - `TRUE` if you have appropriate access and `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#'   library(phslookups)
#'   if (have_access()){
#'     print("Success!")
#'   } else {
#'     print("Maybe next time")
#'   }
#' }
#' @export
have_access <- function(){
  read_access_to_dir(get_lookups_dir())
}
check_access <- function(fail_on_no_access = TRUE){
  if (have_access()){
    cli::cli_inform(c("v" = "You have the appropriate file permissions!"),
                    .frequency = "once",
                    .frequency_id = "lookup_access_inform")
  } else {
    no_access_msg <- c("x" = "You don't have the appropriate file permissions to {get_lookups_dir()}",
                       "i" = "Please raise a ServiceNow request for access to the UNIX acute dataset")
    if (fail_on_no_access){
      cli::cli_abort(no_access_msg)
    } else {
      cli::cli_warn(no_access_msg)
    }
  }
}
read_access_to_dir <- function(path){
  file.access(path, 4) >= 0L
}
