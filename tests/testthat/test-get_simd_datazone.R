test_that("simd_datazone is returned", {
  get_simd_datazone() %>%
    expect_message()

  simd_datazone <- suppressMessages(get_simd_datazone())

  expect_s3_class(simd_datazone, "tbl_df")
  expect_equal(nrow(unique(simd_datazone["hb2019"])), 14)
  expect_equal(nrow(unique(simd_datazone["hscp2019"])), 31)
  expect_equal(nrow(unique(simd_datazone["ca2019"])), 32)
  expect_equal(nrow(unique(simd_datazone["datazone2011"])), 6976)

  expect_named(simd_datazone, simd_datazone_variables)
})
