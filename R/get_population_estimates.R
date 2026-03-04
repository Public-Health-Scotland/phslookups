#' Get population estimates
#'
#' These functions retrieve population estimates for different geographic
#' levels. They read population data from specified files and filter it
#' based on the input parameters. The functions also allow for grouping by
#' age and pivoting the data for wider format.
#'
#' @param version The version of the population estimates to use
#'  (default: "latest").
#' @param min_year,max_year (optional) The minimum and maximum years to
#'  include in the results.
#' @param age_groups Logical, indicating whether to aggregate population
#'  estimates by age groups.
#' If `TRUE`, the `phsmethods::create_age_groups` function is used.
#' @param pivot_wider Optionally reshape the data into a wider format,
#'  summarising population counts by the specified columns.
#'   Allowed values:
#'   * `FALSE` (default): Do not pivot.
#'   * `TRUE` or `"all"`: Pivot by both sex and age/age group.
#'   * `"age"`: Pivot by age/age group only.
#'   * `"age-only"`: Pivot by age/age group and aggregate to remove sex.
#'   * `"sex"`: Pivot by sex only.
#'   * `"sex-only"`: Pivot by sex group and aggregate to remove age/age group
#' @param ... Additional arguments passed to [phsmethods::create_age_groups()].
#'
#' @return A tibble containing the filtered and possibly transformed
#'  population data.
#'
#' @name get_population_estimates
NULL

#' @rdname get_population_estimates
#' @export
get_hb_pop_est <- function(
  version = "latest",
  min_year = NULL,
  max_year = NULL,
  age_groups = FALSE,
  pivot_wider = FALSE,
  ...
) {
  get_pop_est(
    "hb",
    version,
    min_year,
    max_year,
    age_groups,
    pivot_wider,
    ...
  )
}

#' @rdname get_population_estimates
#' @export
get_ca_pop_est <- function(
  version = "latest",
  min_year = NULL,
  max_year = NULL,
  age_groups = FALSE,
  pivot_wider = FALSE,
  ...
) {
  get_pop_est(
    "ca",
    version,
    min_year,
    max_year,
    age_groups,
    pivot_wider,
    ...
  )
}

#' @rdname get_population_estimates
#' @export
get_hscp_pop_est <- function(
  version = "latest",
  min_year = NULL,
  max_year = NULL,
  age_groups = FALSE,
  pivot_wider = FALSE,
  ...
) {
  get_pop_est(
    "hscp",
    version,
    min_year,
    max_year,
    age_groups,
    pivot_wider,
    ...
  )
}

#' @rdname get_population_estimates
#' @export
get_dz_pop_est <- function(
  version = "latest",
  min_year = NULL,
  max_year = NULL,
  age_groups = FALSE,
  pivot_wider = FALSE,
  ...
) {
  get_pop_est(
    "datazone",
    version,
    min_year,
    max_year,
    age_groups,
    pivot_wider,
    ...
  )
}

#' @rdname get_population_estimates
#' @export
get_iz_pop_est <- function(
  version = "latest",
  min_year = NULL,
  max_year = NULL,
  age_groups = FALSE,
  pivot_wider = FALSE,
  ...
) {
  get_pop_est(
    "intzone",
    version,
    min_year,
    max_year,
    age_groups,
    pivot_wider,
    ...
  )
}


#' Read the population file and dispatch to the correct processor
#' @noRd
get_pop_est <- function(
  level,
  version,
  min_year,
  max_year,
  age_groups,
  pivot_wider,
  ...
) {
  pop_dir <- fs::path(get_lookups_dir(), "Populations", "Estimates")

  file_name_re <- paste0(
    level,
    "[0-9]{0,4}_pop_est_[0-9]{4}_[0-9]{4}\\.parquet"
  )

  pop_path <- find_latest_file(
    directory = pop_dir,
    regexp = file_name_re,
    ignore.case = TRUE
  )

  pop_est <- read_file(pop_path)

  if (level %in% c("datazone", "intzone")) {
    process_low_level_pop(
      pop_est,
      level,
      pop_path,
      min_year,
      max_year,
      age_groups,
      pivot_wider,
      ...
    )
  } else {
    process_high_level_pop(
      pop_est,
      pop_path,
      min_year,
      max_year,
      age_groups,
      pivot_wider,
      ...
    )
  }
}

#' Validate and filter years
#' @noRd
validate_years <- function(data, pop_path, min_year, max_year) {
  if (!is.null(min_year) && !is.null(max_year) && min_year > max_year) {
    cli::cli_abort(
      "Invalid years: {.arg min_year} must not be greater than {.arg max_year}"
    )
  }

  if (!is.null(min_year)) {
    min_year_available <- min(data$year)
    if (min_year < min_year_available) {
      cli::cli_abort(
        "{.arg min_year} must be at least {min_year_available} when using the
        {.file {fs::path_file(pop_path)}} file."
      )
    }
    data <- dplyr::filter(data, .data$year >= min_year)
  }

  if (!is.null(max_year)) {
    max_year_available <- max(data$year)
    if (max_year > max_year_available) {
      cli::cli_abort(
        "{.arg max_year} must be at most {max_year_available} when using the
        {.file {fs::path_file(pop_path)}} file."
      )
    }
    data <- dplyr::filter(data, .data$year <= max_year)
  }

  data
}

#' Validate pivot_wider argument
#' @noRd
validate_pivot_wider <- function(pivot_wider) {
  if (!inherits(pivot_wider, "logical")) {
    rlang::arg_match(
      pivot_wider,
      values = c("all", "age", "age-only", "sex", "sex-only")
    )
  } else {
    pivot_wider
  }
}

#' Aggregate wide age columns into age group columns
#' @noRd
apply_age_groups_wide <- function(data, ...) {
  age_cols <- grep("^age", names(data), value = TRUE)
  ages_num <- clean_age_col_names(age_cols)

  groups <- phsmethods::create_age_groups(ages_num, ...)
  group_map <- split(age_cols, groups)

  exprs <- lapply(group_map, function(cols) {
    rlang::expr(rowSums(dplyr::across(dplyr::all_of(!!cols))))
  })

  non_age_cols <- setdiff(names(data), age_cols)

  data |>
    dplyr::mutate(!!!exprs) |>
    dplyr::select(
      dplyr::all_of(non_age_cols),
      dplyr::all_of(names(group_map))
    )
}

#' Clean raw age column names to numeric form
#' @noRd
clean_age_col_names <- function(x) {
  as.integer(gsub("90plus", 90L, gsub("^age", "", x), fixed = TRUE))
}

#' Pivot long data to wider format
#' @noRd
pivot_pop_data <- function(data, id_cols, names_from) {
  tidyr::pivot_wider(
    data,
    id_cols = {{ id_cols }},
    names_from = !!names_from,
    values_from = "pop",
    values_fn = sum,
    names_prefix = "pop_",
    names_repair = janitor::make_clean_names
  )
}


# ── High-Level Processing (HB, CA, HSCP) ───────────────────────

#' Process long-format population data
#' @noRd
process_high_level_pop <- function(
  data,
  pop_path,
  min_year,
  max_year,
  age_groups,
  pivot_wider,
  ...
) {
  pivot_wider <- validate_pivot_wider(pivot_wider)
  data <- validate_years(data, pop_path, min_year, max_year)

  if (age_groups) {
    data <- data |>
      dplyr::mutate(
        age_group = phsmethods::create_age_groups(x = .data$age, ...),
        .keep = "unused"
      ) |>
      dplyr::group_by(dplyr::across(!.data$pop)) |>
      dplyr::summarise(pop = sum(.data$pop), .groups = "drop")
  }

  if (isFALSE(pivot_wider)) {
    return(data)
  }

  age_col <- if (age_groups) "age_group" else "age"

  if (isTRUE(pivot_wider) || identical(pivot_wider, "all")) {
    pivot_pop_data(data, -"sex", c("sex_name", age_col))
  } else if (identical(pivot_wider, "age")) {
    pivot_pop_data(data, c(-"sex", "sex_name"), age_col)
  } else if (identical(pivot_wider, "age-only")) {
    pivot_pop_data(data, c(-"sex", -"sex_name"), age_col)
  } else if (identical(pivot_wider, "sex")) {
    pivot_pop_data(data, c(-"sex", age_col), "sex_name")
  } else if (identical(pivot_wider, "sex-only")) {
    pivot_pop_data(data, c(-"sex", -age_col), "sex_name")
  }
}


# ── Low-Level Processing (DZ, IZ) ──────────────────────────────

#' Process wide-format population data
#'
#' Avoids expensive double-pivoting by staying in the native wide format
#' whenever possible. Only pivots to long when pivot_wider = FALSE.
#' @noRd
process_low_level_pop <- function(
  data,
  level,
  pop_path,
  min_year,
  max_year,
  age_groups,
  pivot_wider,
  ...
) {
  pivot_wider <- validate_pivot_wider(pivot_wider)

  id_col <- ifelse(level == "datazone", "datazone2011", "intzone2011")
  name_col <- ifelse(level == "datazone", "datazone2011name", "intzone2011name")
  geo_cols <- c(id_col, name_col)

  # Select only relevant columns, drop total_pop
  data <- data |>
    dplyr::select(
      "year",
      dplyr::all_of(geo_cols),
      "sex",
      dplyr::starts_with("age")
    ) |>
    dplyr::select(-dplyr::any_of("total_pop"))

  data <- validate_years(data, pop_path, min_year, max_year)

  # Apply age grouping on wide columns before any pivoting
  if (age_groups) {
    data <- apply_age_groups_wide(data, ...)
  }

  # Identify current age/group columns
  meta_cols <- c("year", geo_cols, "sex")
  age_cols <- setdiff(names(data), meta_cols)

  # ── pivot_wider = FALSE: convert to long format ──
  if (isFALSE(pivot_wider)) {
    if (age_groups) {
      return(
        tidyr::pivot_longer(
          data,
          cols = dplyr::all_of(age_cols),
          names_to = "age_group",
          values_to = "pop"
        )
      )
    }

    return(
      data |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(age_cols),
          names_to = "age",
          values_to = "pop",
          names_prefix = "age"
        ) |>
        dplyr::mutate(
          age = dplyr::if_else(
            .data$age == "90plus",
            90L,
            as.integer(.data$age)
          )
        )
    )
  }

  # ── pivot_wider = "age": already wide by age, just rename ──
  if (identical(pivot_wider, "age")) {
    rename_fn <- if (age_groups) {
      function(x) janitor::make_clean_names(paste0("pop_", x))
    } else {
      function(x) {
        janitor::make_clean_names(paste0("pop_", clean_age_col_names(x)))
      }
    }
    return(
      data |>
        dplyr::rename_with(rename_fn, dplyr::all_of(age_cols))
    )
  }

  # ── pivot_wider = "age-only": aggregate sexes, then rename ──
  if (identical(pivot_wider, "age-only")) {
    rename_fn <- if (age_groups) {
      function(x) janitor::make_clean_names(paste0("pop_", x))
    } else {
      function(x) {
        janitor::make_clean_names(paste0("pop_", clean_age_col_names(x)))
      }
    }

    # Vectorised addition of the two sex rows per geo/year
    sexes <- unique(data$sex)
    sex1_data <- data[data$sex == sexes[[1]], ]
    sex2_data <- data[data$sex == sexes[[2]], ]
    sex1_data[age_cols] <- sex1_data[age_cols] + sex2_data[age_cols]

    return(
      sex1_data |>
        dplyr::select(-"sex") |>
        dplyr::rename_with(rename_fn, dplyr::all_of(age_cols))
    )
  }

  # ── pivot_wider = TRUE/"all": spread sex across age columns ──
  if (isTRUE(pivot_wider) || identical(pivot_wider, "all")) {
    # Rename age cols to clean numeric form before pivot
    rename_fn <- if (age_groups) {
      function(x) x
    } else {
      function(x) clean_age_col_names(x)
    }
    data <- dplyr::rename_with(data, rename_fn, dplyr::all_of(age_cols))

    new_age_cols <- rename_fn(age_cols)

    return(
      tidyr::pivot_wider(
        data,
        id_cols = c("year", dplyr::all_of(geo_cols)),
        names_from = "sex",
        values_from = dplyr::all_of(new_age_cols),
        names_glue = "pop_{sex}_{.value}",
        names_repair = janitor::make_clean_names
      )
    )
  }

  # ── pivot_wider = "sex": age as rows, sex as columns ──
  # Unavoidable pivot_longer first, then pivot_wider on sex
  if (identical(pivot_wider, "sex")) {
    age_col_name <- if (age_groups) "age_group" else "age"

    long_data <- if (age_groups) {
      tidyr::pivot_longer(
        data,
        cols = dplyr::all_of(age_cols),
        names_to = "age_group",
        values_to = "pop"
      )
    } else {
      data |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(age_cols),
          names_to = "age",
          values_to = "pop",
          names_prefix = "age"
        ) |>
        dplyr::mutate(
          age = dplyr::if_else(.data$age == "90plus", "90", .data$age),
          age = as.numeric(.data$age)
        )
    }

    return(
      pivot_pop_data(long_data, age_col_name, "sex")
    )
  }

  # ── pivot_wider = "sex-only": sum ages per sex, then spread ──
  if (identical(pivot_wider, "sex-only")) {
    data |>
      dplyr::mutate(pop = rowSums(dplyr::across(dplyr::all_of(age_cols)))) |>
      dplyr::select(-dplyr::all_of(age_cols)) |>
      pivot_pop_data(dplyr::everything(), "sex")
  }
}
