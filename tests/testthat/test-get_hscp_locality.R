test_that("hscp_locality lookup is returned", {
  get_hscp_locality() %>%
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

  expect_named(hscp_locality, hscp_locality_variables)
})
