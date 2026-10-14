test_that("read_file works", {
  rds_path <- tempfile(fileext = ".rds")
  csv_path <- tempfile(fileext = ".csv")
  parquet_path <- tempfile(fileext = ".parquet")

  anscombe <- tibble::tibble(datasets::anscombe)

  readr::write_rds(anscombe, rds_path)
  readr::write_csv(anscombe, csv_path)
  arrow::write_parquet(anscombe, parquet_path)

  expect_identical(anscombe, read_file(rds_path))
  expect_identical(anscombe, read_file(csv_path))
  expect_identical(anscombe, read_file(parquet_path))

  expect_named(read_file(rds_path, col_select = "x1"), "x1")
  expect_named(read_file(csv_path, col_select = "x1"), "x1")
  expect_named(read_file(parquet_path, col_select = "x1"), "x1")
})

test_that("read_file errors on invalid extension", {
  txt_path <- tempfile(fileext = ".txt")
  writeLines("hello", txt_path)
  expect_error(
    read_file(txt_path),
    "Invalid extension"
  )
})

test_that("read_file errors when file does not exist", {
  expect_error(
    read_file(fs::path("nonexistent", "file.csv")),
    "is NOT available"
  )
})
