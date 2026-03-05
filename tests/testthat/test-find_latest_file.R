dir <- fs::dir_create(tempdir(), "find_file_test")
# Generate some files
fs::file_create(fs::path(
  dir,
  apply(expand.grid(letters, 1:9), 1, paste0, collapse = ""),
  ext = "txt"
))

test_that("By default will find the last created file", {
  # Wait a second so the file gets a different timestamp
  Sys.sleep(1)
  fs::file_create(fs::path(dir, "b0.txt"))

  expect_equal(
    find_latest_file(dir, "[a-z]\\d.txt"),
    fs::path(dir, "b0.txt")
  ) |>
    expect_message("\"b0\"")
})

test_that("Can find latest file by file name", {
  expect_equal(
    find_latest_file(dir, "[a-z]\\d.txt", "file_name"),
    fs::path(dir, "z9.txt")
  ) |>
    expect_message("\"z9\"")
})


test_that("Returns error if no files match the regex", {
  empty_dir <- fs::dir_create(tempdir(), "find_file_empty")
  expect_error(
    find_latest_file(empty_dir, "doesnotexist[.]txt"),
    "There was no file"
  )
})

test_that("quiet = TRUE suppresses the info message", {
  dir2 <- fs::dir_create(tempdir(), "find_file_quiet")
  fs::file_create(fs::path(dir2, "a1.txt"))
  expect_silent(
    find_latest_file(dir2, "a1[.]txt", quiet = TRUE)
  )
})

test_that("Returns the only file if only one file exists", {
  dir3 <- fs::dir_create(tempdir(), "find_file_single")
  file <- fs::file_create(fs::path(dir3, "single.txt"))
  expect_equal(
    find_latest_file(dir3, "single[.]txt"),
    fs::path(dir3, "single.txt")
  )
})

test_that("Returns error if directory has no files", {
  dir4 <- fs::dir_create(tempdir(), "find_file_nofiles")
  expect_error(
    find_latest_file(dir4, ".*"),
    "There was no file"
  )
})

test_that("Invalid selection_method falls back to modification_date", {
  dir5 <- fs::dir_create(tempdir(), "find_file_invalid_method")
  fs::file_create(fs::path(dir5, "a1.txt"))
  expect_equal(
    find_latest_file(dir5, "a1[.]txt", selection_method = "invalid"),
    fs::path(dir5, "a1.txt")
  )
})
