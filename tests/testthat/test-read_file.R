test_that("read_file works", {
  rds_path <- tempfile(fileext = ".rds")
  csv_path <- tempfile(fileext = ".csv")
  parquet_path <- tempfile(fileext = ".parquet")

  anscombe <- tibble::tibble(datasets::anscombe)

  readr::write_rds(anscombe, rds_path)
  readr::write_csv(anscombe, csv_path)
  arrow::write_parquet(anscombe, parquet_path)

  expect_equal(anscombe, read_file(rds_path))
  expect_equal(anscombe, read_file(csv_path))
  expect_equal(anscombe, read_file(parquet_path))
})
