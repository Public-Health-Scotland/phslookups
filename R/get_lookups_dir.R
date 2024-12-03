get_lookups_dir <- function() {
  switch(version$os,
    "linux-gnu" = fs::path(
      "/",
      "conf",
      "linkage",
      "output",
      "lookups",
      "Unicode"
    ),
    "mingw32" = fs::path(
      "//",
      "cl-out",
      "lookups",
      "Unicode"
    )
  )
}

get_spd_path <- function(version = "latest", quiet = FALSE) {
  dir <- get_lookups_dir()

  if (version == "latest") {
    find_latest_file(
      directory = dir,
      regexp = "Scottish_Postcode_Directory_\\d{4}_[1-2]\\.parquet",
      selection_method = "file_name",
      quiet = quiet
    )
  } else {
    fs::path(
      dir,
      glue::glue("Scottish_Postcode_Directory_{version}.parquet")
    )
  }
}
