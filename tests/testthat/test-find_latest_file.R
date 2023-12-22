dir <- fs::dir_create(tempdir(), "find_file_test")
# Generate some files
fs::file_create(fs::path(
  dir,
  apply(expand.grid(letters, 1:9), 1, paste0, collapse = ""),
  ext = "txt"
))

test_that("By default will find the last created file", {
  fs::file_create(fs::path(dir, "b0.txt"))

  expect_equal(
    find_latest_file(dir, "[a-z]\\d.txt"),
    fs::path(dir, "b0.txt")
  ) %>%
    expect_message("\"b0.txt\"")
})

test_that("Can find latest file by file name", {
  expect_equal(
    find_latest_file(dir, "[a-z]\\d.txt", "file_name"),
    fs::path(dir, "z9.txt")
  ) %>%
    expect_message("\"z9.txt\"")
})
