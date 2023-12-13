test_that("spd is returned", {
  spd <- get_spd()
  expect_s3_class(spd, "tbl_df")
  expect_equal(names(spd)[1:2], c("pc7", "pc8"))
  expect_equal(nrow(unique(spd["hb2019"])), 14)
  expect_equal(nrow(unique(spd["hscp2019"])), 31)
  expect_equal(nrow(unique(spd["ca2019"])), 32)
  expect_equal(nrow(unique(spd["datazone2011"])), 6976)

  expect_named(spd, spd_variables)
})

test_that("col selection works", {
  expect_named(get_spd(col_select = "pc7"), "pc7")
  expect_named(get_spd(col_select = c("pc7", "pc8")), c("pc7", "pc8"))
})
