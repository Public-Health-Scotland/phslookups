dir <- fs::dir_create(tempdir(), "find_file_test")
# Generate some files
fs::file_create(fs::path(
  dir,
  apply(expand.grid(letters, 1L:9L), 1L, paste0, collapse = ""),
  ext = "csv"
))

test_that("By default will find the last created file", {
  # Wait a second so the file gets a different timestamp
  Sys.sleep(1L)
  fs::file_create(fs::path(dir, "b0.csv"))

  expect_identical(
    find_latest_file(dir, "[a-z]\\d.csv"),
    fs::path(dir, "b0.csv")
  ) |>
    expect_message("\"b0\"")
})

test_that("Can find latest file by file name", {
  expect_identical(
    find_latest_file(dir, "[a-z]\\d.csv", "file_name"),
    fs::path(dir, "z9.csv")
  ) |>
    expect_message("\"z9\"")
})


test_that("Returns error if no files match the regex", {
  empty_dir <- fs::dir_create(tempdir(), "find_file_empty")
  expect_error(
    find_latest_file(empty_dir, "doesnotexist[.]csv"),
    "There was no file"
  )
})

test_that("quiet = TRUE suppresses the info message", {
  dir2 <- fs::dir_create(tempdir(), "find_file_quiet")
  fs::file_create(fs::path(dir2, "a1.csv"))
  expect_silent(
    find_latest_file(dir2, "a1[.]csv", quiet = TRUE)
  )
})

test_that("Returns the only file if only one file exists", {
  dir3 <- fs::dir_create(tempdir(), "find_file_single")
  file <- fs::file_create(fs::path(dir3, "single.csv"))
  expect_message(
    expect_identical(
      find_latest_file(dir3, "single[.]csv"),
      fs::path(dir3, "single.csv")
    ),
    "Using the latest available version:"
  )
})

test_that("Returns error if directory has no files", {
  dir4 <- fs::dir_create(tempdir(), "find_file_nofiles")
  expect_error(
    find_latest_file(dir4, ".*"),
    "There was no file"
  )
})

test_that("Invalid selection_method fails", {
  expect_error(
    find_latest_file(dir5, "a1[.]csv", selection_method = "invalid")
  )
})
