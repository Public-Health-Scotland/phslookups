test_that("Lookup dir is as expected", {
  expect_snapshot(get_lookups_dir(), variant = .Platform[["OS.type"]])
  expect_type(get_lookups_dir(), "character")
  expect_s3_class(get_lookups_dir(), "fs_path")
})


test_that("Lookup dir exists", {
  skip_if_not(have_access())
  expect_true(fs::dir_exists(get_lookups_dir()))
})
