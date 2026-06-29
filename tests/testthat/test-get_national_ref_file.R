test_that("get_national_ref_file validates filename", {
  expect_error(
    get_national_ref_file(NULL),
    "must be a single non-empty character string"
  )

  expect_error(
    get_national_ref_file(character()),
    "must be a single non-empty character string"
  )

  expect_error(
    get_national_ref_file(c("gpprac", "postcode")),
    "must be a single non-empty character string"
  )

  expect_error(
    get_national_ref_file(""),
    "must be a single non-empty character string"
  )
})

test_that("National Reference Files directory exists", {
  skip_on_ci()

  national_ref_dir <- fs::path(
    get_lookups_dir(),
    "National Reference Files"
  )

  expect_true(fs::dir_exists(national_ref_dir))
})

test_that("get_national_ref_file reads a National Reference File", {
  skip_on_ci()

  gpprac <- get_national_ref_file("gpprac")

  expect_s3_class(gpprac, "tbl_df")
  expect_gt(nrow(gpprac), 0L)
  expect_gt(ncol(gpprac), 0L)
})

test_that("get_national_ref_file ignores supplied file extension", {
  skip_on_ci()

  without_ext <- get_national_ref_file("gpprac")
  with_ext <- get_national_ref_file("gpprac.csv")

  expect_identical(with_ext, without_ext)
})

test_that("get_national_ref_file matches case-insensitively", {
  skip_on_ci()

  lower_case <- get_national_ref_file("gpprac")
  mixed_case <- get_national_ref_file("GPprac.csv")

  expect_identical(mixed_case, lower_case)
})

test_that("get_national_ref_file supports col_select with character vectors", {
  skip_on_ci()

  gpprac <- get_national_ref_file(
    "gpprac",
    col_select = c("praccode", "postcode")
  )

  expect_s3_class(gpprac, "tbl_df")
  expect_named(gpprac, c("praccode", "postcode"))
})

test_that("get_national_ref_file supports col_select with tidyselect", {
  skip_on_ci()

  gpprac <- get_national_ref_file(
    "gpprac",
    col_select = dplyr::matches("prac|post", ignore.case = TRUE)
  )

  expect_s3_class(gpprac, "tbl_df")
  expect_true(any(grepl("prac", names(gpprac), ignore.case = TRUE)))
  expect_true(any(grepl("post", names(gpprac), ignore.case = TRUE)))
})

test_that("get_national_ref_file errors when file is not found", {
  skip_on_ci()

  expect_error(
    get_national_ref_file("definitely_not_a_real_national_ref_file"),
    "National Reference File .* was not found"
  )

  expect_error(
    get_national_ref_file("definitely_not_a_real_national_ref_file"),
    "Matching is case-insensitive and ignores the file extension"
  )
})

test_that(
  "get_national_ref_file suggests close matches when stringdist is installed",
  {
    skip_on_ci()
    skip_if_not_installed("stringdist")

    expect_error(
      get_national_ref_file("ggprac"),
      "Did you mean"
    )

    expect_error(
      get_national_ref_file("ggprac"),
      "gpprac.csv"
    )
  }
)
