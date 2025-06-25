test_that("returns a tibble with expected structure and content", {
  expect_message(get_hscp_locality())
  hscp_locality <- suppressMessages(get_hscp_locality())
  expect_s3_class(hscp_locality, "tbl_df")
  expect_equal(
    names(hscp_locality)[c(1, 3)],
    c("datazone2011", "hscp_locality")
  )
  expect_equal(nrow(unique(hscp_locality["hb2019"])), 14)
  expect_equal(nrow(unique(hscp_locality["hscp2019"])), 31)
  expect_equal(nrow(unique(hscp_locality["ca2019"])), 32)
  expect_equal(nrow(unique(hscp_locality["datazone2011"])), 6976)
})

test_that("col_select returns correct columns (vector)", {
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
  all_cols <- suppressMessages(get_hscp_locality(col_select = NULL))
  expect_gt(ncol(all_cols), 2)
})

test_that("invalid version format returns an error", {
  expect_error(
    get_hscp_locality(version = "2024-01-01"),
    "Invalid version name"
  )
  expect_error(get_hscp_locality(version = "abcd"), "Invalid version name")
})

test_that("nonexistent but valid version returns an error", {
  expect_error(get_hscp_locality(version = "20991231"), "file")
})
