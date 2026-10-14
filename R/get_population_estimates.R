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
#' @param output Output format. One of `"long"`, `"age-and-sex"`, `"age"`,
#'   `"age-only"`, `"sex"`, `"sex-only"` or `"total"`. See the
#'   "Output formats" section for details.
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
#' @section Output formats:
#'
#' `output` controls the shape of the returned data.
#'
#' Allowed values are:
#'
#' - `"long"` (default): long format with separate age and sex columns.
#' - `"age-and-sex"`: wide format with separate columns for sex and age.
#' - `"age"`: wide by age, retaining sex.
#' - `"age-only"`: wide by age, aggregating across sex.
#' - `"sex"`: wide by sex, retaining age.
#' - `"sex-only"`: wide by sex, aggregating across age.
#' - `"total"`: one population total (`pop`) per year and area.
#'
#' @return
#' A [tibble][tibble::tibble-package] containing the filtered and possibly
#' transformed population data.
#'
#' The columns returned depend on the geography level requested, and the value
#' of `output`.
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
#' iz_pop <- get_iz_pop_est(output = "sex")
#'
#' # Data Zone population estimates in wider format by age
#' dz_pop <- get_dz_pop_est(output = "age")
#'
#' # Total population by year and area
#' ca_total <- get_ca_pop_est(output = "total")
NULL


#' @describeIn get_population_estimates Retrieve Health Board level population
#' estimates.
#' @export
get_hb_pop_est <- function(
  version = "latest",
  min_year = NULL,
  max_year = NULL,
  output = "long"
) {
  get_pop_est(
    level = "HB",
    version = version,
    min_year = min_year,
    max_year = max_year,
    output = output,
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
  output = "long"
) {
  get_pop_est(
    level = "CA",
    version = version,
    min_year = min_year,
    max_year = max_year,
    output = output,
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
  output = "long"
) {
  get_pop_est(
    level = "HSCP",
    version = version,
    min_year = min_year,
    max_year = max_year,
    output = output,
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
  output = "long"
) {
  get_pop_est(
    level = "IntZone",
    version = version,
    min_year = min_year,
    max_year = max_year,
    output = output,
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
  output = "long"
) {
  get_pop_est(
    level = "DataZone",
    version = version,
    min_year = min_year,
    max_year = max_year,
    output = output,
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
  output,
  call = rlang::caller_call()
) {
  level <- rlang::arg_match0(
    level,
    values = c("HB", "CA", "HSCP", "DataZone", "IntZone")
  )

  output <- rlang::arg_match0(
    output,
    values = c(
      "long",
      "age-and-sex",
      "age",
      "age-only",
      "sex",
      "sex-only",
      "total"
    ),
    arg_nm = "output",
    error_call = call
  )

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

  if (!is.null(min_year) && !is.null(max_year) && min_year > max_year) {
    cli::cli_abort(
      "{.arg min_year} must not be greater than {.arg max_year}.",
      call = call
    )
  }

  pop_dir <- fs::path(get_lookups_dir(), "Populations", "Estimates")

  geog_level_re <- paste0(
    level,
    if (version == "latest") {
      "[0-9]{4}"
    } else {
      version
    }
  )
  shared_file_re <- "_pop_est_[0-9]{4}_[0-9]{4}\\."
  file_name_re <- paste0(geog_level_re, shared_file_re)

  if (level %in% c("DataZone", "IntZone")) {
    # Resolve the latest geography version first, then gather all of its
    # year-range Parquet files.
    if (identical(version, "latest")) {
      pop_path <- find_latest_file(
        directory = pop_dir,
        regexp = file_name_re,
        selection_method = "file_name",
        quiet = TRUE
      )
      version_match <- stringr::str_match(
        fs::path_file(pop_path),
        paste0("^", level, "([0-9]{4})")
      )
      file_name_re <- paste0(
        level,
        version_match[,
          2L
        ],
        shared_file_re
      )
    }

    low_level_paths <- fs::dir_ls(
      path = pop_dir,
      type = "file",
      regexp = paste0(file_name_re, "parquet$"),
      recurse = FALSE
    )

    if (length(low_level_paths) == 0L) {
      cli::cli_abort(
        "There were no Parquet files for {.val {level}}
        version {.val {version}}.",
        call = call
      )
    }

    pop_est <- arrow::open_dataset(low_level_paths, format = "parquet")
  } else {
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
  }

  # Use rename_with as it can work on Arrow data
  # without forcing collection
  pop_est <- pop_est |>
    dplyr::rename_with(janitor::make_clean_names) |>
    # Validate year inputs and filter
    validate_years(min_year = min_year, max_year = max_year, call = call)

  pop_est <- if (level %in% c("DataZone", "IntZone")) {
    process_low_level_pop(data = pop_est, output = output, call = call)
  } else {
    process_high_level_pop(data = pop_est, output = output, call = call)
  }

  if (identical(version, "latest")) {
    # nolint start object_usage_linter
    version_info <- stringr::str_extract(
      fs::path_file(pop_path),
      "^([A-z]+)([0-9]{4})",
      group = c(1L, 2L)
    )

    cli::cli_inform(c(
      v = "Using the latest available geography:
      {.val {version_info[1]}{version_info[2]}}.",
      i = "If you require older boundaries or for reproducibility purposes
      specify {.arg version} explicitly:
      {.code version = \"{version_info[2]}\"}."
    ))
    # nolint end
  }

  pop_est
}

# High-Level Processing (HB, CA, HSCP)
#' Process long-format population data
#' @noRd
process_high_level_pop <- function(data, output, call = rlang::caller_call()) {
  if (is_arrow_data(data)) {
    data <- dplyr::collect(data)
  }

  if (identical(output, "long")) {
    data
  } else if (identical(output, "age-and-sex")) {
    pivot_pop_data(data, !dplyr::any_of("sex"), c("sex_name", "age"))
  } else if (identical(output, "age")) {
    pivot_pop_data(data, !dplyr::any_of("sex"), "age")
  } else if (identical(output, "age-only")) {
    pivot_pop_data(data, !dplyr::any_of(c("sex", "sex_name")), "age")
  } else if (identical(output, "sex")) {
    pivot_pop_data(data, !dplyr::any_of("sex"), "sex_name")
  } else if (identical(output, "sex-only")) {
    pivot_pop_data(data, !dplyr::any_of(c("sex", "age")), "sex_name")
  } else if (identical(output, "total")) {
    id_cols <- setdiff(names(data), c("sex", "sex_name", "age", "pop"))
    data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
      dplyr::summarise(pop = sum(.data$pop), .groups = "drop")
  }
}

# Low-Level Processing (DZ, IZ)
#' Process wide-format population data
#'
#' Avoids expensive double-pivoting by staying in the native wide format
#' whenever possible. Only pivots to long for output = "long".
#' @noRd
process_low_level_pop <- function(data, output, call = rlang::caller_call()) {
  all_cols <- names(data)
  if (any(startsWith(all_cols, "datazone"))) {
    geo_cols <- all_cols[(startsWith(all_cols, "datazone"))]
  } else {
    geo_cols <- all_cols[(startsWith(all_cols, "intzone"))]
  }
  age_cols <- all_cols[(startsWith(all_cols, "age"))]

  # Select only relevant columns, drop total_pop
  data <- data |>
    dplyr::select(
      "year",
      dplyr::all_of(geo_cols),
      sex_name = "sex",
      dplyr::all_of(age_cols)
    )

  # output = "long": convert to long format
  if (identical(output, "long")) {
    if (is_arrow_data(data)) {
      data <- dplyr::collect(data)
    }
    tidyr::pivot_longer(
      data = data,
      cols = dplyr::all_of(age_cols),
      names_to = "age",
      values_to = "pop",
      names_prefix = "age",
      names_transform = list(age = parse_age)
    )
  } else if (identical(output, "age")) {
    if (is_arrow_data(data)) {
      data <- dplyr::collect(data)
    }
    # output = "age": already wide by age, just rename
    dplyr::rename_with(
      data,
      \(col) paste0("pop_", clean_age_col_names(col)),
      dplyr::all_of(age_cols)
    )
  } else if (identical(output, "age-only")) {
    # output = "age-only": aggregate sexes, then rename
    data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("year", geo_cols)))) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(age_cols), sum),
        .groups = "drop"
      ) |>
      dplyr::rename_with(
        \(col) paste0("pop_", clean_age_col_names(col)),
        dplyr::all_of(age_cols)
      ) |>
      dplyr::collect()
  } else if (identical(output, "age-and-sex")) {
    if (is_arrow_data(data)) {
      data <- dplyr::collect(data)
    }
    # output = "age-and-sex": spread sex across age columns

    # Rename age cols to clean numeric form before pivot
    data <- dplyr::rename_with(
      data,
      \(col) {
        clean_age_col_names(col)
      },
      dplyr::all_of(age_cols)
    )

    new_age_cols <- clean_age_col_names(age_cols)

    tidyr::pivot_wider(
      data,
      id_cols = c("year", dplyr::all_of(geo_cols)),
      names_from = "sex_name",
      values_from = dplyr::all_of(new_age_cols),
      names_glue = "pop_{sex_name}_{.value}",
      names_repair = janitor::make_clean_names
    )
  } else if (identical(output, "sex")) {
    if (is_arrow_data(data)) {
      data <- dplyr::collect(data)
    }

    data |>
      tidyr::pivot_wider(
        names_from = "sex_name",
        values_from = dplyr::starts_with("age"),
        names_glue = "{.value}_pop_{tolower(sex_name)}"
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::starts_with("age"),
        names_to = c("age", ".value"),
        names_pattern = "^age(.*)_(pop_[mf])$",
        names_transform = list(age = parse_age)
      )
  } else if (identical(output, "sex-only")) {
    if (is_arrow_data(data)) {
      data <- dplyr::collect(data)
    }

    # output = "sex-only": sum ages per sex, then spread
    data |>
      dplyr::mutate(pop = rowSums(dplyr::pick(dplyr::all_of(age_cols)))) |>
      dplyr::select(-dplyr::all_of(age_cols)) |>
      pivot_pop_data(dplyr::everything(), "sex_name")
  } else if (identical(output, "total")) {
    data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("year", geo_cols)))) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(age_cols), sum),
        .groups = "drop"
      ) |>
      dplyr::collect() |>
      dplyr::mutate(pop = rowSums(dplyr::pick(dplyr::all_of(age_cols)))) |>
      dplyr::select("year", dplyr::all_of(geo_cols), "pop")
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

  available_years_msg <- paste(
    "Available years are {.val {min_year_available}}",
    "to {.val {max_year_available}}."
  )

  lower <- min_year %||% min_year_available
  upper <- max_year %||% max_year_available

  if (lower < min_year_available || lower > max_year_available) {
    cli::cli_abort(
      c(
        "{.arg min_year} must be within the available year range,
      not {.val {min_year}}.",
        i = available_years_msg
      ),
      call = call
    )
  }

  if (upper < min_year_available || upper > max_year_available) {
    cli::cli_abort(
      c(
        "{.arg max_year} must be within the available year range,
      not {.val {max_year}}.",
        i = available_years_msg
      ),
      call = call
    )
  }

  # Apply filter (stays lazy for Arrow; in-memory for data.frame)
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
  inherits(data, c("ArrowTabular", "arrow_dplyr_query"))
}
