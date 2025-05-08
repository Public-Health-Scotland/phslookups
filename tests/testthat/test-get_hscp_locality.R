test_that("hscp_locality lookup is returned", {
  get_hscp_locality() |>
    expect_message()

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

test_that("col selection works", {
  expect_named(
    get_hscp_locality(col_select = "datazone2011"),
    "datazone2011"
  ) |>
    expect_message()
  expect_named(
    get_hscp_locality(col_select = c("datazone2011", "hscp_locality")),
    c("datazone2011", "hscp_locality")
  ) |>
    expect_message()
})

test_that("col selection works with tidyselect", {
  expect_named(
    get_hscp_locality(col_select = dplyr::starts_with("hscp"))
  ) |>
    expect_message()

  expect_named(
    get_hscp_locality(col_select = c(
      dplyr::starts_with("hscp"),
      "datazone2011"
    ))
  ) |>
    expect_message()

  expect_named(
    get_hscp_locality(col_select = c(
      dplyr::starts_with("hscp"),
      dplyr::starts_with("datazone")
    ))
  ) |>
    expect_message()

  expect_named(
    get_hscp_locality(col_select = c(
      "hscp_locality",
      dplyr::matches(".+?2019name$")
    ))
  ) |>
    expect_message()
})
