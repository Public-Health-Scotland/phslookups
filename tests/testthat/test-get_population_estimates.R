skip_on_ci()

test_that("population estimates are returned using defaults", {
  expect_message(
    hb <- get_hb_pop_est(),
    "Using the latest available geography"
  )
  expect_message(
    ca <- get_ca_pop_est(),
    "Using the latest available geography"
  )
  expect_message(
    hscp <- get_hscp_pop_est(),
    "Using the latest available geography"
  )
  expect_message(
    iz <- get_iz_pop_est(),
    "Using the latest available geography"
  )
  expect_message(
    dz <- get_dz_pop_est(),
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
  expect_in(c("year", "sex_name", "age", "pop"), names(hb))
  expect_in(c("year", "sex_name", "age", "pop"), names(ca))
  expect_in(c("year", "sex_name", "age", "pop"), names(hscp))
  expect_in(c("year", "sex_name", "age", "pop"), names(iz))
  expect_in(c("year", "sex_name", "age", "pop"), names(dz))
})

test_that("population estimates are returned for valid versions", {
  expect_s3_class(get_hb_pop_est(version = "2006"), "tbl_df")
  expect_s3_class(get_hb_pop_est(version = "2019"), "tbl_df")

  expect_s3_class(get_ca_pop_est(version = "2019"), "tbl_df")

  expect_s3_class(get_hscp_pop_est(version = "2019"), "tbl_df")

  expect_s3_class(get_iz_pop_est(version = "2001"), "tbl_df")
  expect_s3_class(get_iz_pop_est(version = "2011"), "tbl_df")

  expect_s3_class(get_dz_pop_est(version = "2001"), "tbl_df")
  expect_s3_class(get_dz_pop_est(version = "2011"), "tbl_df")
})

test_that("year filtering works for various versions", {
  hb <- get_hb_pop_est(version = "2019", min_year = 2024L, max_year = 2024L)
  hb_old <- get_hb_pop_est(version = "2006", min_year = 2013L, max_year = 2013L)

  ca <- get_ca_pop_est(version = "2019", min_year = 2024L, max_year = 2024L)

  hscp <- get_hscp_pop_est(version = "2019", min_year = 2024L, max_year = 2024L)

  iz <- get_iz_pop_est(version = "2011", min_year = 2024L, max_year = 2024L)
  iz_old <- get_iz_pop_est(version = "2001", min_year = 2014L, max_year = 2014L)

  dz <- get_dz_pop_est(version = "2011", min_year = 2024L, max_year = 2024L)
  dz_old <- get_dz_pop_est(version = "2001", min_year = 2014L, max_year = 2014L)

  expect_setequal(hb$year, 2024L)
  expect_setequal(hb_old$year, 2013L)
  expect_setequal(ca$year, 2024L)
  expect_setequal(hscp$year, 2024L)
  expect_setequal(iz$year, 2024L)
  expect_setequal(iz_old$year, 2014L)
  expect_setequal(dz$year, 2024L)
  expect_setequal(dz_old$year, 2014L)
})

test_that("high-level pivot_wider options work", {
  hb_true <- get_hb_pop_est(
    version = "2019",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = TRUE
  )
  hb_age <- get_hb_pop_est(
    version = "2019",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = "age"
  )
  hb_age_only <- get_hb_pop_est(
    version = "2019",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = "age-only"
  )
  hb_sex <- get_hb_pop_est(
    version = "2019",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = "sex"
  )
  hb_sex_only <- get_hb_pop_est(
    version = "2019",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = "sex-only"
  )

  expect_s3_class(hb_true, "tbl_df")
  expect_s3_class(hb_age, "tbl_df")
  expect_s3_class(hb_age_only, "tbl_df")
  expect_s3_class(hb_sex, "tbl_df")
  expect_s3_class(hb_sex_only, "tbl_df")

  expect_in("year", names(hb_true))
  expect_in(c("year", "sex_name"), names(hb_age))
  expect_in("year", names(hb_age_only))
  expect_in(c("year", "age"), names(hb_sex))
  expect_in("year", names(hb_sex_only))

  expect_in(c("pop_m", "pop_f"), names(hb_sex_only))
  expect_in(paste0("pop_", 0L:90L), names(hb_age_only))
})

test_that("other high-level geographies pivot successfully", {
  ca_age_only <- get_ca_pop_est(
    version = "2019",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = "age-only"
  )
  ca_sex_only <- get_ca_pop_est(
    version = "2019",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = "sex-only"
  )

  hscp_age_only <- get_hscp_pop_est(
    version = "2019",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = "age-only"
  )
  hscp_sex_only <- get_hscp_pop_est(
    version = "2019",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = "sex-only"
  )

  expect_s3_class(ca_age_only, "tbl_df")
  expect_s3_class(ca_sex_only, "tbl_df")
  expect_s3_class(hscp_age_only, "tbl_df")
  expect_s3_class(hscp_sex_only, "tbl_df")

  expect_in(c("pop_m", "pop_f"), names(ca_sex_only))
  expect_in(c("pop_m", "pop_f"), names(hscp_sex_only))
  expect_gt(sum(startsWith(names(ca_age_only), "pop_")), 0L)
  expect_gt(sum(startsWith(names(hscp_age_only), "pop_")), 0L)
})

test_that("low-level pivot_wider options work", {
  dz_long <- get_dz_pop_est(
    version = "2011",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = FALSE
  )
  dz_true <- get_dz_pop_est(
    version = "2011",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = TRUE
  )
  dz_age <- get_dz_pop_est(
    version = "2011",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = "age"
  )
  dz_age_only <- get_dz_pop_est(
    version = "2011",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = "age-only"
  )
  dz_sex <- get_dz_pop_est(
    version = "2011",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = "sex"
  )
  dz_sex_only <- get_dz_pop_est(
    version = "2011",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = "sex-only"
  )

  expect_s3_class(dz_long, "tbl_df")
  expect_s3_class(dz_true, "tbl_df")
  expect_s3_class(dz_age, "tbl_df")
  expect_s3_class(dz_age_only, "tbl_df")
  expect_s3_class(dz_sex, "tbl_df")
  expect_s3_class(dz_sex_only, "tbl_df")

  expect_in(c("year", "sex_name", "age", "pop"), names(dz_long))
  expect_in("year", names(dz_true))
  expect_in("year", names(dz_age))
  expect_in("year", names(dz_age_only))
  expect_in("year", names(dz_sex))
  expect_in("year", names(dz_sex_only))

  expect_in(c("pop_m", "pop_f"), names(dz_sex_only))
  expect_gt(sum(startsWith(names(dz_age_only), "pop_")), 0L)
})

test_that("intermediate zone pivot_wider options work", {
  iz_long <- get_iz_pop_est(
    version = "2011",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = FALSE
  )
  iz_true <- get_iz_pop_est(
    version = "2011",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = TRUE
  )
  iz_age_only <- get_iz_pop_est(
    version = "2011",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = "age-only"
  )
  iz_sex_only <- get_iz_pop_est(
    version = "2011",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = "sex-only"
  )

  expect_s3_class(iz_long, "tbl_df")
  expect_s3_class(iz_true, "tbl_df")
  expect_s3_class(iz_age_only, "tbl_df")
  expect_s3_class(iz_sex_only, "tbl_df")

  expect_in(c("year", "sex_name", "age", "pop"), names(iz_long))
  expect_in(c("pop_m", "pop_f"), names(iz_sex_only))
  expect_gt(sum(startsWith(names(iz_age_only), "pop_")), 0L)
})

test_that("older low-level pivot_wider options work", {
  iz_old_age_only <- get_iz_pop_est(
    version = "2001",
    min_year = 2014L,
    max_year = 2014L,
    pivot_wider = "age-only"
  )
  iz_old_sex_only <- get_iz_pop_est(
    version = "2001",
    min_year = 2014L,
    max_year = 2014L,
    pivot_wider = "sex-only"
  )

  dz_old_age_only <- get_dz_pop_est(
    version = "2001",
    min_year = 2014L,
    max_year = 2014L,
    pivot_wider = "age-only"
  )
  dz_old_sex_only <- get_dz_pop_est(
    version = "2001",
    min_year = 2014L,
    max_year = 2014L,
    pivot_wider = "sex-only"
  )

  expect_s3_class(iz_old_age_only, "tbl_df")
  expect_s3_class(iz_old_sex_only, "tbl_df")
  expect_s3_class(dz_old_age_only, "tbl_df")
  expect_s3_class(dz_old_sex_only, "tbl_df")

  expect_in(c("pop_m", "pop_f"), names(iz_old_sex_only))
  expect_in(c("pop_m", "pop_f"), names(dz_old_sex_only))
  expect_gt(sum(startsWith(names(iz_old_age_only), "pop_")), 0L)
  expect_gt(sum(startsWith(names(dz_old_age_only), "pop_")), 0L)
})

test_that("high-level pivoted population totals are consistent with long format", {
  long <- get_hb_pop_est(
    version = "2019",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = FALSE
  )

  sex_only <- get_hb_pop_est(
    version = "2019",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = "sex-only"
  )

  expect_identical(
    sum(long$pop, na.rm = TRUE),
    sum(
      unlist(
        dplyr::select(sex_only, dplyr::starts_with("pop_")),
        use.names = FALSE
      ),
      na.rm = TRUE
    )
  )
})

test_that("low-level pivoted population totals are consistent with long format", {
  long <- get_dz_pop_est(
    version = "2011",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = FALSE
  )

  age_only <- get_dz_pop_est(
    version = "2011",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = "age-only"
  )

  expect_identical(
    sum(long$pop, na.rm = TRUE),
    sum(
      unlist(
        dplyr::select(age_only, dplyr::starts_with("pop_")),
        use.names = FALSE
      ),
      na.rm = TRUE
    )
  )
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
  expect_error(
    get_hb_pop_est(version = "1900"),
    "There was no file"
  )
})

test_that("invalid pivot_wider option errors", {
  expect_error(
    get_hb_pop_est(pivot_wider = "bad"),
    "`pivot_wider` must be one of"
  )
})

test_that("invalid year filters error", {
  expect_error(
    get_hb_pop_est(
      version = "2019",
      min_year = 1900L
    ),
    "`min_year` must be at least 1981"
  )

  expect_error(
    get_hb_pop_est(
      version = "2019",
      max_year = 2099L
    ),
    "`max_year` must be at most 2024"
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
