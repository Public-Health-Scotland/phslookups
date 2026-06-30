#' Get population estimates
#'
#' @description
#' Retrieve population estimates for different geographic levels.
#'
#' These functions read population estimate files and filter the data based on
#' the input parameters. The functions also allow for pivoting the data into a
#' wider format.
#'
#' @param version The geography version of the population estimates to use
#'   (default: `"latest"`). For example, for HB2019 use `"2019"`.
#' @param min_year,max_year Optional minimum and maximum years to include in the
#'   results.
#' @param pivot_wider Optionally reshape the data into a wider format,
#'   summarising population counts by the specified columns. See the
#'   "Pivoting population data" section for allowed values.
#'
#' @section Geography versions:
#' `version` controls which geography version of the population estimates is
#' used. The default, `"latest"`, uses the most recent available version.
#'
#' To request a specific geography version, supply a 4-digit year as a character
#' string. For example, use `"2019"` for HB2019.
#'
#' @section Filtering years:
#' `min_year` and `max_year` can be used to restrict the population estimates to
#' a particular year or range of years.
#'
#' If both are supplied, `min_year` must not be greater than `max_year`.
#'
#' @section Pivoting population data:
#'
#' `pivot_wider` controls whether the data is returned in long format or
#' reshaped into a wider format.
#'
#' Allowed values are:
#'
#' - `FALSE`: (default) do not pivot.
#' - `TRUE`: pivot by both sex and age.
#' - `"age"`: pivot by age only.
#' - `"age-only"`: pivot by age and aggregate to remove sex.
#' - `"sex"`: pivot by sex only.
#' - `"sex-only"`: pivot by sex and aggregate to remove age.
#'
#' @return
#' A [tibble][tibble::tibble-package] containing the filtered and possibly
#' transformed population data.
#'
#' The columns returned depend on the geography level requested, and the value
#' of `pivot_wider`.
#'
#' @name get_population_estimates
#'
#' @examples
#' # Health Board population estimates
#' hb_pop <- get_hb_pop_est()
#'
#' # Council Area population estimates for a year range
#' ca_pop <- get_ca_pop_est(min_year = 2018, max_year = 2020)
#'
#' # Intermediate Zone population estimates with sex breakdowns as columns
#' iz_pop <- get_iz_pop_est(pivot_wider = "sex")
#'
#' # Data Zone population estimates in wider format by age
#' dz_pop <- get_dz_pop_est(pivot_wider = "age")
NULL


#' @describeIn get_population_estimates Retrieve Health Board level population
#' estimates.
#' @export
get_hb_pop_est <- function(
  version = "latest",
  min_year = NULL,
  max_year = NULL,
  pivot_wider = FALSE
) {
  get_pop_est(
    level = "HB",
    version = version,
    min_year = min_year,
    max_year = max_year,
    pivot_wider = pivot_wider,
    call = rlang::call_match()
  )
}

#' @describeIn get_population_estimates Retrieve Council Area level population
#' estimates.
#' @export
get_ca_pop_est <- function(
  version = "latest",
  min_year = NULL,
  max_year = NULL,
  pivot_wider = FALSE
) {
  get_pop_est(
    level = "CA",
    version = version,
    min_year = min_year,
    max_year = max_year,
    pivot_wider = pivot_wider,
    call = rlang::call_match()
  )
}

#' @describeIn get_population_estimates Retrieve HSCP (Health and Social Care
#' Partnership) level population estimates.
#' @export
get_hscp_pop_est <- function(
  version = "latest",
  min_year = NULL,
  max_year = NULL,
  pivot_wider = FALSE
) {
  get_pop_est(
    level = "HSCP",
    version = version,
    min_year = min_year,
    max_year = max_year,
    pivot_wider = pivot_wider,
    call = rlang::call_match()
  )
}

#' @describeIn get_population_estimates Retrieve Intermediate Zone level
#' population estimates.
#' @export
get_iz_pop_est <- function(
  version = "latest",
  min_year = NULL,
  max_year = NULL,
  pivot_wider = FALSE
) {
  get_pop_est(
    level = "IntZone",
    version = version,
    min_year = min_year,
    max_year = max_year,
    pivot_wider = pivot_wider,
    call = rlang::call_match()
  )
}

#' @describeIn get_population_estimates Retrieve Data Zone level population
#' estimates.
#' @export
get_dz_pop_est <- function(
  version = "latest",
  min_year = NULL,
  max_year = NULL,
  pivot_wider = FALSE
) {
  get_pop_est(
    level = "DataZone",
    version = version,
    min_year = min_year,
    max_year = max_year,
    pivot_wider = pivot_wider,
    call = rlang::call_match()
  )
}


#' Read the population file and dispatch to the correct processor
#' @noRd
get_pop_est <- function(
  level,
  version,
  min_year,
  max_year,
  pivot_wider,
  call = rlang::caller_call()
) {
  level <- rlang::arg_match0(
    level,
    values = c("HB", "CA", "HSCP", "DataZone", "IntZone")
  )

  if (!is.logical(pivot_wider)) {
    pivot_wider <- rlang::arg_match(
      pivot_wider,
      values = c("age", "age-only", "sex", "sex-only"),
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
  file_name_re <- paste0(geog_level_re, shared_file_re)

  pop_path <- find_latest_file(
    directory = pop_dir,
    regexp = file_name_re,
    selection_method = "file_name",
    quiet = TRUE
  )

  pop_est <- if (fs::path_ext(pop_path) == "parquet") {
    read_file(pop_path, as_data_frame = FALSE)
  } else {
    read_file(pop_path)
  }

  if (identical(version, "latest")) {
    version_info <- stringr::str_extract(
      fs::path_file(pop_path),
      "^([A-z]+)([0-9]{4})",
      group = c(1L, 2L)
    )

    cli::cli_inform(c(
      v = "Using the latest available geography: {.val {version_info[1]}{version_info[2]}}.",
      i = "If you require older boundaries or for reproducibility purposes specify {.arg version} explicitly: {.code version = \"{version_info[2]}\"}."
    ))
  }

  # Use rename_with as it can work on Arrow data without forcing collection
  pop_est <- pop_est |>
    dplyr::rename_with(janitor::make_clean_names) |>
    # Validate year inputs and filter
    validate_years(
      min_year = min_year,
      max_year = max_year,
      call = call
    )

  if (level %in% c("DataZone", "IntZone")) {
    process_low_level_pop(
      data = pop_est,
      level = level,
      min_year = min_year,
      max_year = max_year,
      pivot_wider = pivot_wider,
      call = call
    )
  } else {
    process_high_level_pop(
      data = pop_est,
      min_year = min_year,
      max_year = max_year,
      pivot_wider = pivot_wider,
      call = call
    )
  }
}

# High-Level Processing (HB, CA, HSCP)            ─

#' Process long-format population data
#' @noRd
process_high_level_pop <- function(
  data,
  min_year,
  max_year,
  pivot_wider,
  call = rlang::caller_call()
) {
  if (is_arrow_data(data)) {
    data <- dplyr::collect(data)
  }

  if (isFALSE(pivot_wider)) {
    return(data)
  }

  if (isTRUE(pivot_wider)) {
    pivot_pop_data(data, -"sex", c("sex_name", "age"))
  } else if (identical(pivot_wider, "age")) {
    pivot_pop_data(data, c(-"sex", "sex_name"), "age")
  } else if (identical(pivot_wider, "age-only")) {
    pivot_pop_data(data, c(-"sex", -"sex_name"), "age")
  } else if (identical(pivot_wider, "sex")) {
    pivot_pop_data(data, c(-"sex", "age"), "sex_name")
  } else if (identical(pivot_wider, "sex-only")) {
    pivot_pop_data(data, c(-"sex", -"age"), "sex_name")
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
  min_year,
  max_year,
  pivot_wider,
  call = rlang::caller_call()
) {
  # Select only relevant columns, drop total_pop
  data <- data |>
    dplyr::select(
      "year",
      dplyr::starts_with("datazone"),
      dplyr::starts_with("intzone"),
      sex_name = "sex",
      dplyr::starts_with("age")
    ) |>
    dplyr::select(-dplyr::any_of("total_pop"))

  all_cols <- names(data)

  geo_cols <- all_cols[grepl("^(datazone|intzone)", all_cols)]
  meta_cols <- c("year", geo_cols, "sex_name")
  age_cols <- setdiff(all_cols, meta_cols)

  # pivot_wider = FALSE: convert to long format
  if (isFALSE(pivot_wider)) {
    if (is_arrow_data(data)) {
      data <- dplyr::collect(data)
    }
    return(
      tidyr::pivot_longer(
        data = data,
        cols = dplyr::all_of(age_cols),
        names_to = "age",
        values_to = "pop",
        names_prefix = "age",
        names_transform = list(age = parse_age)
      )
    )
  }

  # pivot_wider = "age": already wide by age, just rename
  if (identical(pivot_wider, "age")) {
    if (is_arrow_data(data)) {
      data <- dplyr::collect(data)
    }
    return(
      dplyr::rename_with(
        data,
        \(col) paste0("pop_", clean_age_col_names(col)),
        dplyr::all_of(age_cols)
      )
    )
  }

  # pivot_wider = "age-only": aggregate sexes, then rename
  if (identical(pivot_wider, "age-only")) {
    age_only_data <- data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("year", geo_cols)))) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(age_cols), sum),
        .groups = "drop"
      ) |>
      dplyr::collect()

    age_only_data <- dplyr::rename_with(
      age_only_data,
      \(col) paste0("pop_", clean_age_col_names(col)),
      dplyr::all_of(age_cols)
    )

    return(age_only_data)
  }

  # pivot_wider = TRUE: spread sex across age columns
  if (isTRUE(pivot_wider)) {
    if (is_arrow_data(data)) {
      data <- dplyr::collect(data)
    }

    # Rename age cols to clean numeric form before pivot
    data <- dplyr::rename_with(
      data,
      \(col) clean_age_col_names(col),
      dplyr::all_of(age_cols)
    )

    new_age_cols <- clean_age_col_names(age_cols)

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
  if (identical(pivot_wider, "sex")) {
    if (is_arrow_data(data)) {
      data <- dplyr::collect(data)
    }

    long_data <- data |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(age_cols),
        names_to = "age",
        values_to = "pop",
        names_prefix = "age"
      ) |>
      dplyr::mutate(age = parse_age(.data$age))

    return(
      pivot_pop_data(long_data, dplyr::everything(), "sex_name")
    )
  }

  # pivot_wider = "sex-only": sum ages per sex, then spread
  if (identical(pivot_wider, "sex-only")) {
    if (is_arrow_data(data)) {
      data <- dplyr::collect(data)
    }

    data |>
      dplyr::mutate(pop = rowSums(dplyr::pick(dplyr::all_of(age_cols)))) |>
      dplyr::select(-dplyr::all_of(age_cols)) |>
      pivot_pop_data(dplyr::everything(), "sex_name")
  }
}

#' Validate and filter years, works faster with parquet
#' @noRd
validate_years <- function(
  data,
  min_year,
  max_year,
  call = rlang::caller_call()
) {
  min_year_provided <- !is.null(min_year)
  max_year_provided <- !is.null(max_year)

  if (!min_year_provided && !max_year_provided) {
    return(data)
  }

  if (min_year_provided && max_year_provided && min_year > max_year) {
    cli::cli_abort(
      "Invalid years: {.arg min_year} must not be greater than {.arg max_year}",
      call = call
    )
  }

  if (is_arrow_data(data)) {
    year_range <- data |>
      dplyr::summarise(
        min_year = min(.data$year),
        max_year = max(.data$year)
      ) |>
      dplyr::collect()

    min_year_available <- year_range$min_year[[1L]]
    max_year_available <- year_range$max_year[[1L]]
  } else {
    years <- data$year
    min_year_available <- min(years)
    max_year_available <- max(years)
  }

  if (min_year_provided && min_year < min_year_available) {
    cli::cli_abort(
      "{.arg min_year} must be at least {min_year_available}.",
      call = call
    )
  }

  if (max_year_provided && max_year > max_year_available) {
    cli::cli_abort(
      "{.arg max_year} must be at most {max_year_available}.",
      call = call
    )
  }

  # Apply filter (stays lazy for Arrow; in-memory for data.frame)
  lower <- min_year %||% min_year_available
  upper <- max_year %||% max_year_available

  dplyr::filter(data, .data$year >= lower, .data$year <= upper)
}

#' Clean raw age column names
#' @noRd
clean_age_col_names <- function(col_name) {
  col_name_num_part <- gsub("^age", "", col_name)
  gsub("90plus", "90", col_name_num_part, fixed = TRUE)
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
    names_from = dplyr::all_of(names_from),
    values_from = "pop",
    values_fn = sum,
    names_prefix = "pop_",
    names_repair = janitor::make_clean_names
  )
}

is_arrow_data <- function(data) {
  inherits(
    data,
    c(
      "ArrowTabular", # Table, Dataset
      "arrow_dplyr_query" # result of dplyr verbs on Arrow
    )
  )
}
