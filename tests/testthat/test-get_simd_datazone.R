test_that("simd_datazone is returned", {
  get_simd_datazone() |>
    expect_message()

  simd_datazone <- suppressMessages(get_simd_datazone())

  expect_s3_class(simd_datazone, "tbl_df")
  expect_equal(nrow(unique(simd_datazone["hb2019"])), 14)
  expect_equal(nrow(unique(simd_datazone["hscp2019"])), 31)
  expect_equal(nrow(unique(simd_datazone["ca2019"])), 32)
  expect_equal(nrow(unique(simd_datazone["datazone2011"])), 6976)
})

test_that("col selection works", {
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
  expect_named(
    get_simd_datazone(col_select = dplyr::starts_with("simd"))
  ) |>
    expect_message()

  expect_named(
    get_simd_datazone(col_select = c(
      dplyr::starts_with("simd"),
      "datazone2011"
    ))
  ) |>
    expect_message()

  expect_named(
    get_simd_datazone(col_select = c(
      dplyr::starts_with("simd"),
      dplyr::starts_with("datazone")
    ))
  ) |>
    expect_message()

  expect_named(
    get_simd_datazone(col_select = dplyr::matches("^simd\\d{4}.+?decile$"))
  ) |>
    expect_message()
})
