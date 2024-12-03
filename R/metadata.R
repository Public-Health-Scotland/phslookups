#' Metadata
#'
#' @returns
#' List of metadata.
#'
#' @importFrom tidyselect all_of
#' @rdname metadata
#' @export
load_metadata <- function() {
  ### Placeholder code
  ### The production code should load the metadata files when this fn is called

  spd_names <- names(arrow::read_parquet(get_spd_path(quiet = TRUE), as_data_frame = FALSE))

  list(
    spd = `names<-`(vector("list", length(spd_names)), spd_names)
  )

  # list(
  #   spd = list(
  #     pc7 = "Postcode7",
  #     pc8 = "Postcode8",
  #     split_char = "Is postcode split?"
  #   ),
  #   simd_dz = list(
  #     col_a = "placeholdera",
  #     col_b = "placeholderb",
  #     col_c = "placeholderc"
  #   )
  # )
}

# This object is to be replaced in .onLoad()
#' @rdname metadata
#' @export
.metadata <- NULL
