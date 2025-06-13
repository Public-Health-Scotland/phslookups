test_that("Lookup dir is as exepected", {
  expect_snapshot(get_lookups_dir(), variant = version$os)
  expect_type(get_lookups_dir(), "character")
  expect_s3_class(get_lookups_dir(), "fs_path")
})
