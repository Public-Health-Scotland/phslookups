test_that("simd_postcode is returned", {
  get_simd_postcode() %>%
    expect_message()

  simd_postcode <- suppressMessages(get_simd_postcode())

  expect_s3_class(simd_postcode, "tbl_df")
  expect_equal(names(simd_postcode)[1:2], c("pc8", "pc7"))
  expect_equal(nrow(unique(simd_postcode["hb2019"])), 14)
  expect_equal(nrow(unique(simd_postcode["hscp2019"])), 31)
  expect_equal(nrow(unique(simd_postcode["ca2019"])), 32)
  expect_equal(nrow(unique(simd_postcode["datazone2011"])), 6976)

  expect_named(simd_postcode, simd_postcode_variables)
})

test_that("col selection works", {
  expect_named(
    get_simd_postcode(col_select = "pc7"),
    "pc7"
  ) %>%
    expect_message()
  expect_named(
    get_simd_postcode(col_select = c("pc7", "pc8")),
    c("pc7", "pc8")
  ) %>%
    expect_message()
})
