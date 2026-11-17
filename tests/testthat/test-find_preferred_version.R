test_that("find_preferred_version prefers parquet over rds and csv", {
  tmp_dir <- fs::file_temp("pref_ver_test")
  fs::dir_create(tmp_dir)

  fs::file_create(fs::path(
    tmp_dir,
    c("file1.parquet", "file1.rds", "file1.csv")
  ))

  res <- find_preferred_version(tmp_dir, "file1")
  expect_identical(res, fs::path(tmp_dir, "file1.parquet"))
})

test_that("find_preferred_version falls back correctly", {
  tmp_dir <- fs::file_temp("pref_ver_test2")
  fs::dir_create(tmp_dir)

  fs::file_create(fs::path(tmp_dir, c("file1.rds", "file1.csv")))
  res_rds <- find_preferred_version(tmp_dir, "file1")
  expect_identical(res_rds, fs::path(tmp_dir, "file1.rds"))

  fs::file_delete(fs::path(tmp_dir, "file1.rds"))
  res_csv <- find_preferred_version(tmp_dir, "file1")
  expect_identical(res_csv, fs::path(tmp_dir, "file1.csv"))
})

test_that("find_preferred_version returns NA when no matching file exists", {
  tmp_dir <- fs::file_temp("pref_ver_test3")
  fs::dir_create(tmp_dir)

  res <- find_preferred_version(tmp_dir, "nonexistent")
  expect_identical(as.character(res), NA_character_)
})
