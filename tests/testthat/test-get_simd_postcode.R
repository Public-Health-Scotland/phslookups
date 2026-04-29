skip_on_ci()

test_that("simd_postcode is returned", {
  get_simd_postcode() |>
    expect_message()

  simd_postcode <- suppressMessages(get_simd_postcode())

  expect_s3_class(simd_postcode, "tbl_df")
  expect_identical(names(simd_postcode)[1L:2L], c("pc8", "pc7"))
  expect_identical(nrow(unique(simd_postcode["hb2019"])), 14L)
  expect_identical(nrow(unique(simd_postcode["hscp2019"])), 31L)
  expect_identical(nrow(unique(simd_postcode["ca2019"])), 32L)
  expect_identical(nrow(unique(simd_postcode["datazone2011"])), 6976L)
})

test_that("col selection works", {
  expect_named(
    get_simd_postcode(col_select = "pc7"),
    "pc7"
  ) |>
    expect_message()
  expect_named(
    get_simd_postcode(col_select = c("pc7", "pc8")),
    c("pc7", "pc8")
  ) |>
    expect_message()
})

test_that("col selection works with tidyselect", {
  expect_named(
    get_simd_postcode(col_select = c("pc7", dplyr::starts_with("simd")))
  ) |>
    expect_message()

  expect_named(
    get_simd_postcode(col_select = dplyr::matches("pc[78]")),
    c("pc7", "pc8"),
    ignore.order = TRUE
  ) |>
    expect_message()
})


test_that("invalid postcode_version or simd_version format errors", {
  expect_error(
    get_simd_postcode(postcode_version = "abcd", simd_version = "2020v2"),
    "Invalid version specification"
  )
  expect_error(
    get_simd_postcode(postcode_version = "2020_1", simd_version = "abcd"),
    "Invalid version specification"
  )
})

test_that("col_select = NULL returns all columns", {
  all_cols <- suppressMessages(get_simd_postcode(col_select = NULL))
  expect_gt(ncol(all_cols), 2L)
})

test_that("nonexistent but valid version errors", {
  expect_error(
    get_simd_postcode(postcode_version = "2099_1", simd_version = "2099"),
    "is NOT available" # should error about file not found
  )
})

test_that("using only one version as latest errors", {
  expect_error(
    get_simd_postcode(postcode_version = "2020_1", simd_version = "latest"),
    "both"
  )
  expect_error(
    get_simd_postcode(postcode_version = "latest", simd_version = "2020v2"),
    "both"
  )
})
