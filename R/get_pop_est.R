#' Get population estimates
#'
#' This function retrieves population estimates based on various parameters.
#' It reads population data from a specified file and filters it based on the
#' input parameters. The function also allows for grouping by age and pivoting
#' the data for wider format.
#' @param level The geographic level for which to retrieve population estimates.
#' One of "datazone", "intzone", "hscp", "ca", or "hb".
#' @param version The version of the population estimates to use (default: "latest").
#' @param min_year,max_year (optional) The minimum and maximum years to include in the results.
#' @param age_groups Logical, indicating whether to aggregate population estimates by age groups.
#' If `TRUE`, the `phsmethods::create_age_groups` function is used.
#' @param pivot_wider Optionally reshape the data into a wider format, summarising population counts by the specified columns.
#'   Allowed values:
#'   * `FALSE` (default): Do not pivot.
#'   * `TRUE` or `"all"`: Pivot by both sex and age/age group.
#'   * `"age"`: Pivot by age/age group only.
#'   * `"age-only"`: Pivot by age/age group and aggregate to remove sex.
#'   * `"sex"`: Pivot by sex only.
#'   * `"sex-only"`: Pivot by sex group and aggregate to remove age/age group
#' @param ... Additional arguments passed to [phsmethods::create_age_groups()].
#'
#' @return A tibble containing the filtered and possibly transformed population data.
#'
#' @note
#' Depending on the values for `age_groups` and `pivot_wider`, the resulting
#' columns in the returned tibble will vary. Refer to the examples below for
#' illustration.
#'
#' @export
#'
#' @examples
#' # Basic Usage: Datazone Population Estimates (no filtering)
#' get_pop_est("datazone")
#'
#' # Filter by Year:
#' get_pop_est("ca", min_year = 1995, max_year = 2020)
#'
#' # Age Groups: Health Board (HB) Population Estimates by Age Group
#' get_pop_est("hb", age_groups = TRUE)
#'
#' # Age Groups with Custom Settings:
#' # Aggregate into 5-year age groups, with an open-ended final group "85+"
#' get_pop_est("hb", age_groups = TRUE, by = 5, to = 85)
#'
#' # Pivot Wider (All): CA Population Estimates, Reshaped by Sex and Age Group
#' # The result will have columns for each combination of sex and age group,
#' # e.g., "pop_f_0_4", "pop_m_5_9", etc.
#' get_pop_est("ca", age_groups = TRUE, pivot_wider = "all")
#'
#' # Pivot Wider (Age Only): CA Population Estimates, Reshaped by Age Group Only
#' # This is useful if you only need the total population for each age group, regardless of sex.
#' get_pop_est("ca", age_groups = TRUE, pivot_wider = "age-only")
#'
#' # Combined Filtering, Age Groups, and Pivoting:
#' # CA population from 2015-2020, aggregated by 10-year age groups, and pivoted by sex
#' # The result will have columns for each sex ("pop_f", "pop_m") and a row per age group.
#' get_pop_est("ca", min_year = 2015, max_year = 2020, age_groups = TRUE, by = 10, pivot_wider = "sex")
get_pop_est <- function(
    level = c("datazone", "intzone", "hscp", "ca", "hb"),
    version = "latest",
    min_year = NULL,
    max_year = NULL,
    age_groups = FALSE,
    pivot_wider = FALSE,
    ...) {
  level <- rlang::arg_match(level)
  if (!inherits(pivot_wider, "logical")) {
    pivot_wider <- rlang::arg_match(
      pivot_wider,
      values = c("all", "age", "age-only", "sex", "sex-only")
    )
  }

  ext <- "rds"
  pop_dir <- fs::path(get_lookups_dir(), "Populations", "Estimates")

  file_name_re <- paste0(level, "[0-9]{0,4}_pop_est_[0-9]{4}_[0-9]{4}\\.", ext)

  pop_path <- find_latest_file(
    directory = pop_dir,
    regexp = file_name_re,
    ignore.case = TRUE
  )

  pop_est <- read_file(pop_path)

  # Validate year range and filter
  if (!is.null(min_year) && !is.null(max_year) && min_year > max_year) {
    cli::cli_abort(
      "Invalid years: {.arg min_year} must not be greater than {.arg max_year}"
    )
  }

  if (!is.null(min_year)) {
    min_year_available <- min(pop_est$year)
    if (min_year < min_year_available) {
      cli::cli_abort(
        "{.arg min_year} must be at least {min_year_available} when using the
        {.file {fs::path_file(pop_path)}} file."
      )
    }
    pop_est <- dplyr::filter(pop_est, .data$year >= min_year)
  }

  if (!is.null(max_year)) {
    max_year_available <- max(pop_est$year)
    if (max_year > max_year_available) {
      cli::cli_abort(
        "{.arg max_year} must be at most {max_year_available} when using the
        {.file {fs::path_file(pop_path)}} file."
      )
    }
    pop_est <- dplyr::filter(pop_est, .data$year <= max_year)
  }

  # Create age groups
  if (age_groups) {
    pop_est <- pop_est |>
      dplyr::mutate(
        age_group = phsmethods::create_age_groups(x = .data$age, ...),
        .keep = "unused"
      ) |>
      dplyr::group_by(dplyr::across(!.data$pop)) |>
      dplyr::summarise(pop = sum(.data$pop), .groups = "drop")
  }

  # Pivot data
  if (pivot_wider %in% list(TRUE, "all")) {
    pop_est <- pop_est |>
      pivot_data(
        id_cols = -"sex",
        names_from = c("sex_name", dplyr::if_else(age_groups, "age_group", "age"))
      )
  } else if (pivot_wider == "sex") {
    pop_est <- pop_est |>
      pivot_data(
        id_cols = c(-"sex", dplyr::if_else(age_groups, "age_group", "age")),
        names_from = "sex_name"
      )
  } else if (pivot_wider == "sex-only") {
    pop_est <- pop_est |>
      pivot_data(
        id_cols = c(-"sex", -dplyr::if_else(age_groups, "age_group", "age")),
        names_from = "sex_name"
      )
  } else if (pivot_wider == "age") {
    pop_est <- pop_est |>
      pivot_data(
        id_cols = c(-"sex", "sex_name"),
        names_from = dplyr::if_else(age_groups, "age_group", "age")
      )
  } else if (pivot_wider == "age-only") {
    pop_est <- pop_est |>
      pivot_data(
        id_cols = c(-"sex", -"sex_name"),
        names_from = dplyr::if_else(age_groups, "age_group", "age")
      )
  }

  return(pop_est)
}


# Helper function to pivot data
pivot_data <- function(data, id_cols, names_from) {
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
