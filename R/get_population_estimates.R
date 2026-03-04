#' Get population estimates
#'
#' These functions retrieve population estimates for different geographic
#' levels. They read population data from specified files and filter it
#' based on the input parameters. The functions also allow for grouping by
#' age and pivoting the data for wider format.
#'
#' @param version The geography version of the population estimates to use
#'  (default: "latest"). For example for HB2019 use `"2019"`.
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
  call <- rlang::call_match()
  get_pop_est(
    level = "HB",
    version = version,
    min_year = min_year,
    max_year = max_year,
    age_groups = age_groups,
    pivot_wider = pivot_wider,
    call = call,
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
  call <- rlang::call_match()
  get_pop_est(
    level = "CA",
    version = version,
    min_year = min_year,
    max_year = max_year,
    age_groups = age_groups,
    pivot_wider = pivot_wider,
    call = call,
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
  call <- rlang::call_match()
  get_pop_est(
    level = "HSCP",
    version = version,
    min_year = min_year,
    max_year = max_year,
    age_groups = age_groups,
    pivot_wider = pivot_wider,
    call = call,
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
  call <- rlang::call_match()
  get_pop_est(
    level = "DataZone",
    version = version,
    min_year = min_year,
    max_year = max_year,
    age_groups = age_groups,
    pivot_wider = pivot_wider,
    call = call,
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
  call <- rlang::call_match()
  get_pop_est(
    level = "IntZone",
    version = version,
    min_year = min_year,
    max_year = max_year,
    age_groups = age_groups,
    pivot_wider = pivot_wider,
    call = call,
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
  call = rlang::caller_call(),
  ...
) {
  level <- rlang::arg_match0(
    level,
    values = c("HB", "CA", "HSCP", "DataZone", "IntZone")
  )
  is_low_level <- level %in% c("DataZone", "IntZone")

  if (!inherits(pivot_wider, "logical")) {
    pivot_wider <- rlang::arg_match(
      pivot_wider,
      values = c("all", "age", "age-only", "sex", "sex-only"),
      error_call = call
    )
  }

  if (!identical(version, "latest")) {
    version <- trimws(version)
    if (!grepl("^[0-9]{4}$", version)) {
      cli::cli_abort(
        c(
          "{.arg version} must be {.val latest} or a 4-digit year.",
          i = "You supplied {.val {version}}."
        ),
        call = call
      )
    }
  }

  pop_dir <- fs::path(get_lookups_dir(), "Populations", "Estimates")

  geog_level_re <- paste0(
    level,
    if (version == "latest") "[0-9]{4}" else version
  )
  shared_file_re <- "_pop_est_[0-9]{4}_[0-9]{4}\\."
  ext_re <- if (version == "latest") "parquet" else "(csv|rds|parquet)"
  file_name_re <- paste0(geog_level_re, shared_file_re, ext_re, "$")

  pop_path <- find_latest_file(
    directory = pop_dir,
    regexp = file_name_re,
    quiet = version != "latest"
  )

  pop_est <- if (fs::path_ext(pop_path) == "parquet") {
    read_file(pop_path, as_data_frame = FALSE)
  } else {
    read_file(pop_path)
  }

  pop_est <- standardise_col_names(pop_est)

  if (level %in% c("DataZone", "IntZone")) {
    process_low_level_pop(
      data = pop_est,
      level = level,
      pop_path = pop_path,
      min_year = min_year,
      max_year = max_year,
      age_groups = age_groups,
      pivot_wider = pivot_wider,
      call = call,
      ...
    )
  } else {
    process_high_level_pop(
      data = pop_est,
      pop_path = pop_path,
      min_year = min_year,
      max_year = max_year,
      age_groups = age_groups,
      pivot_wider = pivot_wider,
      call = call,
      ...
    )
  }
}

# High-Level Processing (HB, CA, HSCP)            ─

#' Process long-format population data
#' @noRd
process_high_level_pop <- function(
  data,
  pop_path,
  min_year,
  max_year,
  age_groups,
  pivot_wider,
  call = rlang::caller_call(),
  ...
) {
  data <- validate_years(
    data = data,
    pop_path = pop_path,
    min_year = min_year,
    max_year = max_year,
    call = call
  )

  if (inherits(data, c("ArrowTabular", "arrow_dplyr_query"))) {
    data <- dplyr::collect(data)
  }

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


# Low-Level Processing (DZ, IZ)

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
  call = rlang::caller_call(),
  ...
) {
  id_col <- if (level == "DataZone") "datazone2011" else "intzone2011"
  name_col <- if (level == "DataZone") "datazone2011name" else "intzone2011name"
  geo_cols <- c(id_col, name_col)

  # Select only relevant columns, drop total_pop
  data <- data |>
    dplyr::select(
      "year",
      dplyr::all_of(geo_cols),
      sex_name = "sex",
      dplyr::starts_with("age")
    ) |>
    dplyr::select(-dplyr::any_of("total_pop"))

  data <- validate_years(
    data = data,
    pop_path = pop_path,
    min_year = min_year,
    max_year = max_year,
    call = call
  )

  if (inherits(data, c("ArrowTabular", "arrow_dplyr_query"))) {
    data <- dplyr::collect(data)
  }

  # Apply age grouping on wide columns before any pivoting
  if (age_groups) {
    data <- apply_age_groups_wide(data, ...)
  }

  # Identify current age/group columns
  meta_cols <- c("year", geo_cols, "sex_name")
  age_cols <- setdiff(names(data), meta_cols)

  # pivot_wider = FALSE: convert to long format
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
        dplyr::mutate(age = parse_age(.data$age))
    )
  }

  # pivot_wider = "age": already wide by age, just rename
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

  # pivot_wider = "age-only": aggregate sexes, then rename
  if (identical(pivot_wider, "age-only")) {
    rename_fn <- if (age_groups) {
      function(x) janitor::make_clean_names(paste0("pop_", x))
    } else {
      function(x) {
        janitor::make_clean_names(paste0("pop_", clean_age_col_names(x)))
      }
    }

    # Vectorised addition of the two sex rows per geo/year
    sex_m_data <- data[data$sex_name == "M", ]
    sex_f_data <- data[data$sex_name == "F", ]
    sex_m_data[age_cols] <- sex_m_data[age_cols] + sex_f_data[age_cols]

    return(
      sex_m_data |>
        dplyr::select(-"sex_name") |>
        dplyr::rename_with(rename_fn, dplyr::all_of(age_cols))
    )
  }

  # pivot_wider = TRUE/"all": spread sex across age columns
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
        names_from = "sex_name",
        values_from = dplyr::all_of(new_age_cols),
        names_glue = "pop_{sex_name}_{.value}",
        names_repair = janitor::make_clean_names
      )
    )
  }

  # pivot_wider = "sex": age as rows, sex as columns
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
        dplyr::mutate(age = parse_age(.data$age))
    }

    return(
      pivot_pop_data(long_data, dplyr::everything(), "sex_name")
    )
  }

  # pivot_wider = "sex-only": sum ages per sex, then spread
  if (identical(pivot_wider, "sex-only")) {
    data |>
      dplyr::mutate(pop = rowSums(dplyr::across(dplyr::all_of(age_cols)))) |>
      dplyr::select(-dplyr::all_of(age_cols)) |>
      pivot_pop_data(dplyr::everything(), "sex_name")
  }
}

#' Fix column names, old files use upper case Year
#' @noRd
standardise_col_names <- function(data, call = rlang::caller_call()) {
  col_names <- colnames(data)

  # Find a year-ish column ignoring case
  year_col_pos <- which(tolower(col_names) == "Year")

  # If it's not already exactly "year", rename it
  if (!identical(year_col_pos, integer())) {
    data <- dplyr::rename(data, year = .data$Year)
  }

  data
}

#' Validate and filter years, works faster with parquet
#' @noRd
validate_years <- function(
  data,
  pop_path,
  min_year,
  max_year,
  call = rlang::caller_call()
) {
  if (is.null(min_year) && is.null(max_year)) {
    return(data)
  }

  if (!is.null(min_year) && !is.null(max_year) && min_year > max_year) {
    cli::cli_abort(
      "Invalid years: {.arg min_year} must not be greater than {.arg max_year}",
      call = call
    )
  }

  # helper: are we working with an Arrow object?
  is_arrow <- inherits(
    data,
    c(
      "ArrowTabular", # Table, Dataset
      "arrow_dplyr_query" # result of dplyr verbs on Arrow
    )
  )

  if (is_arrow) {
    year_range <- data |>
      dplyr::summarise(
        min_year = min(.data$year),
        max_year = max(.data$year)
      ) |>
      dplyr::collect()

    min_year_available <- year_range$min_year[[1]]
    max_year_available <- year_range$max_year[[1]]
  } else {
    years <- data$year
    min_year_available <- min(years)
    max_year_available <- max(years)
  }

  if (!is.null(min_year) && min_year < min_year_available) {
    cli::cli_abort(
      "{.arg min_year} must be at least {min_av} when using the
       {.file {fs::path_file(pop_path)}} file.",
      call = call
    )
  }

  if (!is.null(max_year) && max_year > max_year_available) {
    cli::cli_abort(
      "{.arg max_year} must be at most {max_av} when using the
       {.file {fs::path_file(pop_path)}} file.",
      call = call
    )
  }

  # Apply filter (stays lazy for Arrow; in-memory for data.frame)
  lower <- if (is.null(min_year)) min_year_available else min_year
  upper <- if (is.null(max_year)) max_year_available else max_year

  dplyr::filter(data, .data$year >= lower, .data$year <= upper)
}

#' Aggregate wide age columns into age group columns
#' @noRd
apply_age_groups_wide <- function(data, ...) {
  age_cols <- grep("^age", names(data), value = TRUE)
  ages_num <- clean_age_col_names(age_cols)

  groups <- phsmethods::create_age_groups(ages_num, ...)
  group_map <- split(age_cols, groups)

  exprs <- lapply(group_map, function(cols) {
    rlang::expr(rowSums(dplyr::pick(dplyr::all_of(!!cols))))
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
  as.integer(gsub("90plus", "90", gsub("^age", "", x), fixed = TRUE))
}

#' Parse ages into an integer, dealing with 90plus
#' @noRd
parse_age <- function(age) {
  age <- dplyr::if_else(age == "90plus", "90", age)
  as.integer(age)
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
