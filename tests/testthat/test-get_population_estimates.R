setup_mock_pop_dir <- function() {
  mock_dir <- fs::file_temp("mock_pop_dir")
  pop_dir <- fs::path(mock_dir, "Populations", "Estimates")
  fs::dir_create(pop_dir)

  # Mock high-level file (HB)
  mock_hb <- tibble::tibble(
    year = rep(2020L:2021L, each = 4L),
    hb2019 = "S08000015",
    hb2019name = "NHS Ayrshire and Arran",
    sex = rep(c(1L, 2L), times = 4L),
    sex_name = rep(c("Male", "Female"), times = 4L),
    age = rep(c(0L, 1L), each = 2L, times = 2L),
    pop = c(100L, 105L, 110L, 115L, 102L, 107L, 112L, 117L)
  )
  arrow::write_parquet(
    mock_hb,
    fs::path(pop_dir, "HB2019_pop_est_1981_2021.parquet")
  )

  # Mock low-level split files (IntZone) to test arrow::open_dataset
  mock_iz1 <- tibble::tibble(
    year = 2020L,
    intzone2011 = "S02000001",
    intzone2011name = "IZ1",
    sex = c("F", "M"),
    age0 = c(10L, 12L),
    age1 = c(15L, 18L),
    age90plus = c(2L, 1L),
    total_pop = c(27L, 31L)
  )
  mock_iz2 <- tibble::tibble(
    year = 2021L,
    intzone2011 = "S02000001",
    intzone2011name = "IZ1",
    sex = c("F", "M"),
    age0 = c(11L, 13L),
    age1 = c(16L, 19L),
    age90plus = c(3L, 1L),
    total_pop = c(30L, 33L)
  )
  arrow::write_parquet(
    mock_iz1,
    fs::path(pop_dir, "IntZone2011_pop_est_2020_2020.parquet")
  )
  arrow::write_parquet(
    mock_iz2,
    fs::path(pop_dir, "IntZone2011_pop_est_2021_2021.parquet")
  )

  mock_dir
}

test_that("all high-level output options work with mock data", {
  mock_dir <- setup_mock_pop_dir()
  testthat::local_mocked_bindings(
    get_lookups_dir = function() mock_dir,
    .package = "phslookups"
  )

  outputs <- c(
    "long",
    "age-and-sex",
    "age",
    "age-only",
    "sex",
    "sex-only",
    "total"
  )

  for (opt in outputs) {
    expect_snapshot({
      cat("Output option:", opt, "\n")
      res <- get_hb_pop_est(output = opt)
      print(dim(res))
      print(names(res))
    })

    expect_s3_class(res, "tbl_df")
    expect_gt(nrow(res), 0L)
  }
})

test_that("all low-level output options work with split mock parquet files", {
  mock_dir <- setup_mock_pop_dir()
  testthat::local_mocked_bindings(
    get_lookups_dir = function() mock_dir,
    .package = "phslookups"
  )

  outputs <- c(
    "long",
    "age-and-sex",
    "age",
    "age-only",
    "sex",
    "sex-only",
    "total"
  )

  for (opt in outputs) {
    expect_snapshot({
      cat("Output option:", opt, "\n")
      res <- get_iz_pop_est(output = opt)
      print(dim(res))
      print(names(res))
    })

    expect_s3_class(res, "tbl_df")
    expect_gt(nrow(res), 0L)
  }
})

test_that("high-level and low-level population totals consistency on mock data", {
  mock_dir <- setup_mock_pop_dir()
  testthat::local_mocked_bindings(
    get_lookups_dir = function() mock_dir,
    .package = "phslookups"
  )

  # High level consistency
  expect_snapshot({
    hb_long <- get_hb_pop_est(output = "long")
    hb_sex_only <- get_hb_pop_est(output = "sex-only")
  })

  expect_identical(
    sum(hb_long$pop, na.rm = TRUE),
    sum(
      unlist(dplyr::select(hb_sex_only, dplyr::starts_with("pop_"))),
      na.rm = TRUE
    )
  )

  # Low level consistency
  expect_snapshot({
    iz_long <- get_iz_pop_est(output = "long")
    iz_age_only <- get_iz_pop_est(output = "age-only")
  })

  expect_identical(
    sum(iz_long$pop, na.rm = TRUE),
    sum(
      unlist(dplyr::select(iz_age_only, dplyr::starts_with("pop_"))),
      na.rm = TRUE
    )
  )
})

test_that("year filtering works on mock data", {
  mock_dir <- setup_mock_pop_dir()
  testthat::local_mocked_bindings(
    get_lookups_dir = function() mock_dir,
    .package = "phslookups"
  )

  expect_snapshot({
    hb_2020 <- get_hb_pop_est(min_year = 2020L, max_year = 2020L)
    iz_2021 <- get_iz_pop_est(min_year = 2021L, max_year = 2021L)
  })

  expect_setequal(hb_2020$year, 2020L)
  expect_setequal(iz_2021$year, 2021L)
})

test_that("invalid version format errors", {
  expect_error(
    get_hb_pop_est(version = "foo"),
    "`version` must be \"latest\" or a 4-digit year"
  )

  expect_error(
    get_hb_pop_est(version = "19"),
    "`version` must be \"latest\" or a 4-digit year"
  )

  expect_error(
    get_hb_pop_est(version = "201"),
    "`version` must be \"latest\" or a 4-digit year"
  )
})

test_that("valid-looking but unavailable version errors", {
  mock_dir <- setup_mock_pop_dir()
  testthat::local_mocked_bindings(
    get_lookups_dir = function() mock_dir,
    .package = "phslookups"
  )

  expect_error(
    get_hb_pop_est(version = "1900"),
    "There was no file"
  )
})

test_that("invalid output option errors", {
  expect_error(
    get_hb_pop_est(output = "bad"),
    "`output` must be one of"
  )
})

test_that("year filters outside the available range error", {
  mock_dir <- setup_mock_pop_dir()
  testthat::local_mocked_bindings(
    get_lookups_dir = function() mock_dir,
    .package = "phslookups"
  )

  expect_error(
    get_hb_pop_est(
      version = "2019",
      min_year = 2019L
    ),
    "`min_year` must be within the available year range"
  )

  expect_error(
    get_hb_pop_est(
      version = "2019",
      min_year = 2022L
    ),
    "`min_year` must be within the available year range"
  )

  expect_error(
    get_hb_pop_est(
      version = "2019",
      max_year = 2019L
    ),
    "`max_year` must be within the available year range"
  )

  expect_error(
    get_hb_pop_est(
      version = "2019",
      max_year = 2022L
    ),
    "`max_year` must be within the available year range"
  )
})

test_that("contradictory year filters error", {
  expect_error(
    get_hb_pop_est(
      version = "2019",
      min_year = 2024L,
      max_year = 2023L
    ),
    "must not be greater than"
  )
})

test_that("population estimates are returned using defaults on real files", {
  skip_if_not(have_access())

  expect_message(
    hb <- get_hb_pop_est(min_year = 2024L, max_year = 2024L),
    "Using the latest available geography"
  )
  expect_message(
    ca <- get_ca_pop_est(min_year = 2024L, max_year = 2024L),
    "Using the latest available geography"
  )
  expect_message(
    hscp <- get_hscp_pop_est(min_year = 2024L, max_year = 2024L),
    "Using the latest available geography"
  )
  expect_message(
    iz <- get_iz_pop_est(min_year = 2024L, max_year = 2024L),
    "Using the latest available geography"
  )
  expect_message(
    dz <- get_dz_pop_est(min_year = 2024L, max_year = 2024L),
    "Using the latest available geography"
  )

  expect_s3_class(hb, "tbl_df")
  expect_s3_class(ca, "tbl_df")
  expect_s3_class(hscp, "tbl_df")
  expect_s3_class(iz, "tbl_df")
  expect_s3_class(dz, "tbl_df")

  expect_gt(nrow(hb), 0L)
  expect_gt(nrow(ca), 0L)
  expect_gt(nrow(hscp), 0L)
  expect_gt(nrow(iz), 0L)
  expect_gt(nrow(dz), 0L)

  expect_in(c("year", "sex_name", "age", "pop"), names(hb))
  expect_in(c("year", "sex_name", "age", "pop"), names(ca))
  expect_in(c("year", "sex_name", "age", "pop"), names(hscp))
  expect_in(c("year", "sex_name", "age", "pop"), names(iz))
  expect_in(c("year", "sex_name", "age", "pop"), names(dz))
})

test_that("population estimates are returned for valid versions on real files", {
  skip_if_not(have_access())

  expect_s3_class(
    get_hb_pop_est(version = "2006", min_year = 2013L, max_year = 2013L),
    "tbl_df"
  )
  expect_s3_class(
    get_hb_pop_est(version = "2019", min_year = 2024L, max_year = 2024L),
    "tbl_df"
  )
  expect_s3_class(
    get_ca_pop_est(version = "2019", min_year = 2024L, max_year = 2024L),
    "tbl_df"
  )
  expect_s3_class(
    get_hscp_pop_est(version = "2019", min_year = 2024L, max_year = 2024L),
    "tbl_df"
  )
  expect_s3_class(
    get_iz_pop_est(version = "2001", min_year = 2014L, max_year = 2014L),
    "tbl_df"
  )
  expect_s3_class(
    get_iz_pop_est(version = "2011", min_year = 2024L, max_year = 2024L),
    "tbl_df"
  )
  expect_s3_class(
    get_dz_pop_est(version = "2001", min_year = 2014L, max_year = 2014L),
    "tbl_df"
  )
  expect_s3_class(
    get_dz_pop_est(version = "2011", min_year = 2024L, max_year = 2024L),
    "tbl_df"
  )
})
