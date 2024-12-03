.onLoad <- function(libname, pkgname) {

  rlang::env_binding_unlock(env = asNamespace("phslookups"), nms = ".metadata")
  assign('.metadata', load_metadata(), envir = asNamespace("phslookups"))
  rlang::env_binding_lock(env = asNamespace("phslookups"), nms = ".metadata")

  check_lookups_access(fail_on_no_access = FALSE)
}
