#' Get population estimates
#'
#' @param level one of "datazone", "intzone", "hscp", "ca" or "hb"
#' @param version default is "latest"
#' @param min_year,max_year (optional) filter years
#' @param age_groups should age groups be used
#' @param ... arguments passed to [phsmethods::create_age_groups()]
#'
#' @return the pop data as a tibble
#' @export
#'
#' @examples
#' get_pop_est("datazone")
#' get_pop_est("hb", min_year = 1995, max_year = 2020)
#' get_pop_est("ca", age_groups = TRUE, by = 10)
get_pop_est <- function(
    level = c("datazone", "intzone", "hscp", "ca", "hb"),
    version = "latest",
    min_year = NULL,
    max_year = NULL,
    age_groups = FALSE,
    ...) {
  level <- rlang::arg_match(level)
  ext <- "rds"
  pop_dir <- fs::path(get_lookups_dir(), "Populations", "Estimates")

  file_name_re <- paste0(level, "[0-9]{0,4}_pop_est_[0-9]{4}_[0-9]{4}\\.", ext)

  pop_path <- find_latest_file(
    directory = pop_dir,
    regexp = file_name_re,
    ignore.case = TRUE
  )

  pop_est <- read_file(pop_path)

  # Year range validation
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
    pop_est <- pop_est[pop_est$year >= min_year, ]
  }

  if (!is.null(max_year)) {
    max_year_available <- max(pop_est$year)
    if (max_year > max_year_available) {
      cli::cli_abort(
        "{.arg max_year} must be at most {max_year_available} when using the
        {.file {fs::path_file(pop_path)}} file."
      )
    }
    pop_est <- pop_est[pop_est$year <= max_year, ]
  }

  if (age_groups) {
    pop_est <- pop_est |>
      dplyr::mutate(
        age_group = phsmethods::create_age_groups(x = .data$age, ...),
        .keep = "unused"
      ) |>
      dplyr::group_by(dplyr::across(!pop)) |>
      dplyr::summarise(pop = sum(pop), .groups = "drop")
  }

  return(pop_est)
}
