context("CSV download functions work correctly")
test_that("download_by_state works properly", {
  want_tables <- c("PLOT", "TREE", "SUBPLOT", "COND", "SURVEY")

  csv_files <- download_by_state(
    state = "ND",
    file_dir = tempdir(),
    files = want_tables
  )

  expect_named(csv_files, expected = want_tables)
  expect_true(
    all(purrr::map_lgl(csv_files, .f = ~ tools::file_ext(.x) == "csv"))
  )
})

test_that("download_and_unzip works properly", {
  url <- "https://apps.fs.usda.gov/fia/datamart/CSV/VT_PLOT.zip"
  local_file <- file.path(
    tempdir(),
    "VT_PLOT.csv"
  )

  downloaded <- download_and_unzip(url = url, file_dir = tempdir())
  expect_length(local_file, n = 1)
  expect_identical(local_file, downloaded)

  bad_url <- "https://apps.fs.usda.gov/fia/datamart/CSV/ZZ_PLOT.zip"
  expect_error(
    download_and_unzip(url = bad_url, file_dir = tempdir())
  )
})
