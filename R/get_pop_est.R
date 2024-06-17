get_pop_est <- function(
    level = c("datazone", "intzone", "hscp", "ca", "hb"),
    version = "latest") {
  level <- rlang::arg_match(level)
  ext <- "rds"
  pop_dir <- fs::path(get_lookups_dir(), "Populations", "Estimates")

  file_name_re <- paste0(level, "[0-9]{0,4}_pop_est_[0-9]{4}_[0-9]{4}\\.", ext)

  pop_path <- find_latest_file(
    directory = pop_dir,
    regexp = file_name_re,
    ignore.case = TRUE
  )

  return(read_file(pop_path))
}
