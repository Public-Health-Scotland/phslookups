mock_dir <- fs::file_temp("mock_lookups")
fs::dir_create(fs::path(mock_dir, "Archive"))

# Create mock files for different lookup types
fs::file_create(
  fs::path(mock_dir, "Archive", c(
    "Scottish_Postcode_Directory_2023_1.parquet",
    "Scottish_Postcode_Directory_2023_1.csv",
    "HSCP Localities_DZ11_Lookup_20230804.csv",
    "DataZone_2011_simd2020v2.rds",
    "postcode_2023_2_simd2020v2.parquet",
    "postcode_2023_2_simd2020v2.csv"
  ))
)

test_that("find_specific_file works for SPD and prioritizes parquet over csv", {
  result <- find_specific_file(
    version = "2023_1",
    directory = mock_dir,
    lookup_type = "SPD"
  )
  expect_true(fs::file_exists(result))
  expect_equal(fs::path_file(result), "Scottish_Postcode_Directory_2023_1.parquet")
})

test_that("find_specific_file works for HSCP Locality", {
  result <- find_specific_file(
    version = "20230804",
    directory = mock_dir,
    lookup_type = "HSCP Locality"
  )
  expect_true(fs::file_exists(result))
  expect_equal(fs::path_file(result), "HSCP Localities_DZ11_Lookup_20230804.csv")
})

test_that("find_specific_file works for SIMD DataZone", {
  result <- find_specific_file(
    version = list(datazone_version = "2011", simd_version = "2020v2"),
    directory = mock_dir,
    lookup_type = "SIMD DataZone"
  )
  expect_true(fs::file_exists(result))
  expect_equal(fs::path_file(result), "DataZone_2011_simd2020v2.rds")
})
# prioritises
test_that("find_specific_file works prioritises parquet over csv", {
  result <- find_specific_file(
    version = list(postcode_version = "2023_2", simd_version = "2020v2"),
    directory = mock_dir,
    lookup_type = "SIMD Postcode"
  )
  expect_true(fs::file_exists(result))
  expect_equal(fs::path_file(result), "postcode_2023_2_simd2020v2.parquet")
})

test_that("find_specific_file errors on unsupported lookup_type", {
  expect_error(
    find_specific_file(
      version = "2023_1",
      directory = mock_dir,
      lookup_type = "UnsupportedType"
    ),
    "Unsupported lookup_type"
  )
})

test_that("find_specific_file errors when file is not found", {
  expect_error(
    find_specific_file(
      version = "2025_1",
      directory = mock_dir,
      lookup_type = "SPD"
    ),
    "version .* is NOT available"
  )
})
