get_lookups_dir <- function() {
  switch(version$os,
    "linux-gnu" = fs::path("/", "conf", "linkage", "output", "lookups", "Unicode"),
    "mingw32" = fs::path("//", "cl-out", "lookups", "Unicode")
  )
}
