get_lookups_dir <- function() {
  switch(
    .Platform$OS.type,
    "unix" = fs::path(
      "/",
      "conf",
      "linkage",
      "output",
      "lookups",
      "Unicode"
    ),
    "windows" = fs::path(
      "//",
      "cl-out",
      "lookups",
      "Unicode"
    )
  )
}
