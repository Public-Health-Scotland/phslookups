test_that("invalid simd_version format errors", {
  expect_error(
    get_simd_datazone(simd_version = "abcd"),
    "Invalid version specification"
  )
  expect_error(
    get_simd_datazone(simd_version = "2024-01-01"),
    "Invalid version specification"
  )
})

test_that("simd_datazone is returned", {
  skip_if_not(have_access())

  get_simd_datazone() |>
    expect_message()

  simd_datazone <- suppressMessages(get_simd_datazone())

  expect_s3_class(simd_datazone, "tbl_df")
  expect_length(unique(simd_datazone[["hb2019"]]), 14L)
  expect_length(unique(simd_datazone[["hscp2019"]]), 31L)
  expect_length(unique(simd_datazone[["ca2019"]]), 32L)
  expect_length(unique(simd_datazone[["datazone2011"]]), 6976L)
})

test_that("col selection works", {
  skip_if_not(have_access())

  expect_named(
    get_simd_datazone(col_select = "datazone2011"),
    "datazone2011"
  ) |>
    expect_message()
  expect_named(
    get_simd_datazone(
      col_select = c("datazone2011", "hscp2019", "simd2020v2_rank")
    ),
    c("datazone2011", "hscp2019", "simd2020v2_rank")
  ) |>
    expect_message()
})

test_that("col selection works with tidyselect", {
  skip_if_not(have_access())

  expect_named(
    get_simd_datazone(col_select = dplyr::starts_with("simd"))
  ) |>
    expect_message()

  expect_named(
    get_simd_datazone(
      col_select = c(
        dplyr::starts_with("simd"),
        "datazone2011"
      )
    )
  ) |>
    expect_message()

  expect_named(
    get_simd_datazone(
      col_select = c(
        dplyr::starts_with("simd"),
        dplyr::starts_with("datazone")
      )
    )
  ) |>
    expect_message()

  expect_named(
    get_simd_datazone(col_select = dplyr::matches("^simd\\d{4}.+?decile$"))
  ) |>
    expect_message()
})

test_that("col_select = NULL returns all columns", {
  skip_if_not(have_access())

  all_cols <- suppressMessages(get_simd_datazone(col_select = NULL))
  expect_gt(ncol(all_cols), 2L)
})

test_that("nonexistent but valid simd_version errors", {
  skip_if_not(have_access())

  expect_error(
    get_simd_datazone(simd_version = "2099"),
    "does NOT exist"
  )
})
