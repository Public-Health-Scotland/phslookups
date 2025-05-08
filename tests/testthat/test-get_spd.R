test_that("spd is returned", {
  get_spd() |>
    expect_message()

  spd <- suppressMessages(get_spd())

  expect_s3_class(spd, "tbl_df")
  expect_equal(names(spd)[1:2], c("pc7", "pc8"))
  expect_equal(nrow(unique(spd["hb2019"])), 14)
  expect_equal(nrow(unique(spd["hscp2019"])), 31)
  expect_equal(nrow(unique(spd["ca2019"])), 32)
  expect_equal(nrow(unique(spd["datazone2011"])), 6976)
})

test_that("col selection works", {
  expect_named(
    get_spd(col_select = "pc7"),
    "pc7"
  ) |>
    expect_message()
  expect_named(
    get_spd(col_select = c("pc7", "pc8")),
    c("pc7", "pc8")
  ) |>
    expect_message()
})

test_that("col selection works with tidyselect", {
  expect_named(
    get_spd(col_select = c("pc7", dplyr::starts_with("hb")))
  ) |>
    expect_message()

  expect_named(
    get_spd(col_select = dplyr::matches("pc[78]")),
    c("pc7", "pc8")
  ) |>
    expect_message()
})


test_that("reading from archive works", {
  expect_s3_class(get_spd(version = "2024_1"), "tbl_df")
  expect_error(get_spd(version = "2010_1"))
  expect_error(get_spd(version = "20243"))
})
