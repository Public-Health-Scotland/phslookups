skip_on_ci()

test_that("spd is returned", {
  get_spd() |>
    expect_message("Using the latest available version") |>
    expect_message("Metadata has been attached ")

  spd <- suppressMessages(get_spd())

  expect_s3_class(spd, "tbl_df")
  expect_identical(names(spd)[1L:2L], c("pc7", "pc8"))
  expect_identical(nrow(unique(spd["hb2019"])), 14L)
  expect_identical(nrow(unique(spd["hscp2019"])), 31L)
  expect_identical(nrow(unique(spd["ca2019"])), 32L)
  expect_identical(nrow(unique(spd["datazone2011"])), 6976L)
})

test_that("col selection works", {
  expect_named(
    get_spd(col_select = "pc7"),
    "pc7"
  ) |>
    expect_message("Using the latest available version") |>
    expect_message("Metadata has been attached ")
  expect_named(
    get_spd(col_select = c("pc7", "pc8")),
    c("pc7", "pc8")
  ) |>
    expect_message("Using the latest available version") |>
    expect_message("Metadata has been attached ")
})

test_that("col selection works with tidyselect", {
  expect_named(
    get_spd(col_select = c("pc7", dplyr::starts_with("hb")))
  ) |>
    expect_message("Using the latest available version") |>
    expect_message("Metadata has been attached ")

  expect_named(
    get_spd(col_select = dplyr::matches("pc[78]")),
    c("pc7", "pc8")
  ) |>
    expect_message("Using the latest available version") |>
    expect_message("Metadata has been attached ")
})


test_that("reading from archive works", {
  get_spd(version = "2024_1") |>
    expect_s3_class("tbl_df") |>
    expect_warning("Metadata is correct for the latest version ") |>
    expect_message("Metadata has been attached ")
  expect_error(get_spd(version = "2010_1", "SPD version .+? is NOT available"))
  expect_error(get_spd(version = "20243"), "Invalid version name:")
})

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

test_that("col_select = NULL returns all columns", {
  all_cols <- suppressMessages(get_spd(col_select = NULL))
  expect_gt(ncol(all_cols), 2L)
})

test_that("nonexistent but valid version errors", {
  expect_error(
    get_spd(version = "2099_1"),
    "is NOT available" # should error about file not found
  )
})
