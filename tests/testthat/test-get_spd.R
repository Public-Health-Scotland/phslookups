test_that("invalid version format errors", {
  expect_error(
    get_spd(version = "abcd"),
    "Invalid version name"
  )
  expect_error(
    get_spd(version = "2024-1"),
    "Invalid version name"
  )
})

test_that("spd is returned", {
  skip_if_not(have_access())

  get_spd() |>
    expect_message("Using the latest available version") |>
    expect_message("Metadata is available ")

  spd <- suppressMessages(get_spd())

  expect_s3_class(spd, "tbl_df")

  expect_length(unique(spd[["hb2019"]]), 14L)
  expect_length(unique(spd[["hscp2019"]]), 31L)
  expect_length(unique(spd[["ca2019"]]), 32L)
  expect_length(unique(spd[["datazone2011"]]), 6976L)
})

test_that("col selection works", {
  skip_if_not(have_access())

  expect_named(
    get_spd(col_select = "pc7"),
    "pc7"
  ) |>
    expect_message("Using the latest available version") |>
    expect_message("Metadata is available ")
  expect_named(
    get_spd(col_select = c("pc7", "pc8")),
    c("pc7", "pc8")
  ) |>
    expect_message("Using the latest available version") |>
    expect_message("Metadata is available ")
})

test_that("col selection works with tidyselect", {
  skip_if_not(have_access())

  expect_named(
    get_spd(col_select = c("pc7", dplyr::starts_with("hb")))
  ) |>
    expect_message("Using the latest available version") |>
    expect_message("Metadata is available ")

  expect_named(
    get_spd(col_select = dplyr::matches("pc[78]")),
    c("pc7", "pc8")
  ) |>
    expect_message("Using the latest available version") |>
    expect_message("Metadata is available ")
})

test_that("reading from archive works", {
  skip_if_not(have_access())

  get_spd(version = "2024_1") |>
    expect_s3_class("tbl_df") |>
    expect_message("Metadata is available ")
  expect_error(get_spd(version = "2010_1"), "version .+? is NOT available")
  expect_error(get_spd(version = "20243"), "Invalid version name:")
})

test_that("col_select = NULL returns all columns", {
  skip_if_not(have_access())

  all_cols <- suppressMessages(get_spd(col_select = NULL))
  expect_gt(ncol(all_cols), 2L)
})

test_that("nonexistent but valid version errors", {
  skip_if_not(have_access())

  expect_error(
    get_spd(version = "2099_1"),
    "is NOT available"
  )
})
