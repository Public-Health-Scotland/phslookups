have_access <- function() {
  file.access(get_lookups_dir(), 4) >= 0L
}
check_lookups_access <- function(fail_on_no_access = TRUE) {
  if (!have_access()) {
    no_access_msg <- c(
 "x" = "You don't have the appropriate file permissions to {get_lookups_dir()}",
 "i" = "Please raise a ServiceNow request for access to the UNIX acute dataset"
    )
    if (fail_on_no_access) {
      cli::cli_abort(no_access_msg)
    } else {
      cli::cli_warn(no_access_msg)
    }
  }
}
