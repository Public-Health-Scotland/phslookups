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

  expect_in("year", names(hb))
  expect_in("year", names(ca))
  expect_in("year", names(hscp))
  expect_in("year", names(iz))
  expect_in("year", names(dz))
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

test_that("latest version emits an informative message", {
  expect_message(
    get_hb_pop_est(version = "latest", min_year = 2024L, max_year = 2024L),
    "Using the latest available geography"
  )
  expect_message(
    get_ca_pop_est(version = "latest", min_year = 2024L, max_year = 2024L),
    "Using the latest available geography"
  )
  expect_message(
    get_hscp_pop_est(version = "latest", min_year = 2024L, max_year = 2024L),
    "Using the latest available geography"
  )
  expect_message(
    get_iz_pop_est(version = "latest", min_year = 2024L, max_year = 2024L),
    "Using the latest available geography"
  )
  expect_message(
    get_dz_pop_est(version = "latest", min_year = 2024L, max_year = 2024L),
    "Using the latest available geography"
  )
})

test_that("year filtering works for supported versions", {
  hb <- get_hb_pop_est(version = "2019", min_year = 2024L, max_year = 2024L)
  hb_old <- get_hb_pop_est(version = "2006", min_year = 2013L, max_year = 2013L)

  ca <- get_ca_pop_est(version = "2019", min_year = 2024L, max_year = 2024L)

  hscp <- get_hscp_pop_est(version = "2019", min_year = 2024L, max_year = 2024L)

  iz <- get_iz_pop_est(version = "2011", min_year = 2024L, max_year = 2024L)
  iz_old <- get_iz_pop_est(version = "2001", min_year = 2014L, max_year = 2014L)

  dz <- get_dz_pop_est(version = "2011", min_year = 2024L, max_year = 2024L)
  dz_old <- get_dz_pop_est(version = "2001", min_year = 2014L, max_year = 2014L)

  expect_setequal(unique(hb$year), 2024L)
  expect_setequal(unique(hb_old$year), 2013L)
  expect_setequal(unique(ca$year), 2024L)
  expect_setequal(unique(hscp$year), 2024L)
  expect_setequal(unique(iz$year), 2024L)
  expect_setequal(unique(iz_old$year), 2014L)
  expect_setequal(unique(dz$year), 2024L)
  expect_setequal(unique(dz_old$year), 2014L)
})

test_that("min_year and max_year work independently", {
  hb_min <- get_hb_pop_est(version = "2019", min_year = 2024L)
  hb_max <- get_hb_pop_est(version = "2019", max_year = 2024L)

  iz_min <- get_iz_pop_est(version = "2011", min_year = 2024L)
  iz_max <- get_iz_pop_est(version = "2011", max_year = 2024L)

  dz_min <- get_dz_pop_est(version = "2011", min_year = 2024L)
  dz_max <- get_dz_pop_est(version = "2011", max_year = 2024L)

  expect_s3_class(hb_min, "tbl_df")
  expect_s3_class(hb_max, "tbl_df")
  expect_s3_class(iz_min, "tbl_df")
  expect_s3_class(iz_max, "tbl_df")
  expect_s3_class(dz_min, "tbl_df")
  expect_s3_class(dz_max, "tbl_df")

  expect_true(all(hb_min$year >= 2024L))
  expect_true(all(hb_max$year <= 2024L))

  expect_true(all(iz_min$year >= 2024L))
  expect_true(all(iz_max$year <= 2024L))

  expect_true(all(dz_min$year >= 2024L))
  expect_true(all(dz_max$year <= 2024L))
})

test_that("high-level pivot_wider options work", {
  hb_long <- get_hb_pop_est(
    version = "2019",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = FALSE
  )
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

  expect_s3_class(hb_long, "tbl_df")
  expect_s3_class(hb_true, "tbl_df")
  expect_s3_class(hb_age, "tbl_df")
  expect_s3_class(hb_age_only, "tbl_df")
  expect_s3_class(hb_sex, "tbl_df")
  expect_s3_class(hb_sex_only, "tbl_df")

  expect_in(c("year", "sex_name", "age", "pop"), names(hb_long))
  expect_in("year", names(hb_true))
  expect_in("year", names(hb_age))
  expect_in("year", names(hb_age_only))
  expect_in("year", names(hb_sex))
  expect_in("year", names(hb_sex_only))

  expect_in(c("pop_m", "pop_f"), names(hb_sex_only))
  expect_gt(sum(startsWith(names(hb_age_only), "pop_")), 0L)
})

test_that("older high-level pivot_wider options work", {
  hb_old_long <- get_hb_pop_est(
    version = "2006",
    min_year = 2013L,
    max_year = 2013L,
    pivot_wider = FALSE
  )
  hb_old_true <- get_hb_pop_est(
    version = "2006",
    min_year = 2013L,
    max_year = 2013L,
    pivot_wider = TRUE
  )
  hb_old_age <- get_hb_pop_est(
    version = "2006",
    min_year = 2013L,
    max_year = 2013L,
    pivot_wider = "age"
  )
  hb_old_age_only <- get_hb_pop_est(
    version = "2006",
    min_year = 2013L,
    max_year = 2013L,
    pivot_wider = "age-only"
  )
  hb_old_sex <- get_hb_pop_est(
    version = "2006",
    min_year = 2013L,
    max_year = 2013L,
    pivot_wider = "sex"
  )
  hb_old_sex_only <- get_hb_pop_est(
    version = "2006",
    min_year = 2013L,
    max_year = 2013L,
    pivot_wider = "sex-only"
  )

  expect_s3_class(hb_old_long, "tbl_df")
  expect_s3_class(hb_old_true, "tbl_df")
  expect_s3_class(hb_old_age, "tbl_df")
  expect_s3_class(hb_old_age_only, "tbl_df")
  expect_s3_class(hb_old_sex, "tbl_df")
  expect_s3_class(hb_old_sex_only, "tbl_df")

  expect_in(c("year", "sex_name", "age", "pop"), names(hb_old_long))
  expect_in(c("pop_m", "pop_f"), names(hb_old_sex_only))
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

test_that("long-format population estimates have expected core columns", {
  hb <- get_hb_pop_est(
    version = "2019",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = FALSE
  )

  iz <- get_iz_pop_est(
    version = "2011",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = FALSE
  )

  dz <- get_dz_pop_est(
    version = "2011",
    min_year = 2024L,
    max_year = 2024L,
    pivot_wider = FALSE
  )

  expect_in(c("year", "sex_name", "age", "pop"), names(hb))
  expect_in(c("year", "sex_name", "age", "pop"), names(iz))
  expect_in(c("year", "sex_name", "age", "pop"), names(dz))
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
