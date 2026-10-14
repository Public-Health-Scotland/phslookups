test_that("invalid version format returns an error", {
  expect_error(
    get_hscp_locality(version = "2024-01-01"),
    "Invalid version name"
  )
  expect_error(
    get_hscp_locality(version = "abcd"),
    "Invalid version name"
  )
})

test_that("returns a tibble with expected structure and content", {
  skip_if_not(have_access())

  expect_message(get_hscp_locality())
  hscp_locality <- suppressMessages(get_hscp_locality())

  expect_s3_class(hscp_locality, "tbl_df")

  expect_identical(
    names(hscp_locality)[c(1L, 3L)],
    c("datazone2011", "hscp_locality")
  )
  expect_length(unique(hscp_locality[["hb2019"]]), 14L)
  expect_length(unique(hscp_locality[["hscp2019"]]), 31L)
  expect_length(unique(hscp_locality[["ca2019"]]), 32L)
  expect_length(unique(hscp_locality[["datazone2011"]]), 6976L)
})

test_that("col_select returns correct columns (vector)", {
  skip_if_not(have_access())

  expect_message(
    expect_named(get_hscp_locality(col_select = "datazone2011"), "datazone2011")
  )
  expect_message(
    expect_named(
      get_hscp_locality(col_select = c("datazone2011", "hscp_locality")),
      c("datazone2011", "hscp_locality")
    )
  )
})

test_that("col_select returns correct columns (tidyselect)", {
  skip_if_not(have_access())

  expect_message(expect_named(get_hscp_locality(
    col_select = dplyr::starts_with("hscp")
  )))
  expect_message(expect_named(get_hscp_locality(
    col_select = c(dplyr::starts_with("hscp"), "datazone2011")
  )))
  expect_message(expect_named(get_hscp_locality(
    col_select = c(dplyr::starts_with("hscp"), dplyr::starts_with("datazone"))
  )))
  expect_message(expect_named(get_hscp_locality(
    col_select = c("hscp_locality", dplyr::matches(".+?2019name$"))
  )))
})

test_that("col_select = NULL returns all columns", {
  skip_if_not(have_access())

  all_cols <- suppressMessages(get_hscp_locality(col_select = NULL))
  expect_gt(ncol(all_cols), 2L)
})

test_that("nonexistent but valid version returns an error", {
  skip_if_not(have_access())

  expect_error(get_hscp_locality(version = "20991231"), "is NOT available")
})
