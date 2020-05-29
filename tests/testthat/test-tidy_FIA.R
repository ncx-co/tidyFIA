context("tidy_fia is working properly")
test_that("tidy_fia produces expected output when postgis = TRUE", {
  expect_error(
    tidy_fia(aoi = NULL, states = NULL, table_names = c("plot", "tree"))
  )

  aoi <- spData::us_states %>%
    dplyr::filter(NAME == "Minnesota") %>%
    sf::st_sample(size = 1, type = "random") %>%
    sf::st_sf() %>%
    sf::st_transform(2163) %>%
    sf::st_buffer(20000)

  want_tables <- c("plot", "subplot", "cond", "survey", "tree")

  fia_data <- tidy_fia(
    aoi = aoi,
    postgis = TRUE,
    table_names = want_tables
  )

  expect_s3_class(
    fia_data,
    "tidyFIA"
  )

  expect_named(
    fia_data,
    expected = c(
      want_tables,
      "aoi"
    ),
    ignore.order = TRUE
  )

  expect_true(
    nrow(fia_data[["tree"]]) > 0
  )

  expect_s3_class(
    fia_data[["plot"]],
    "sf"
  )

  expect_true(
    all(fia_data[["tree"]][["plt_cn"]] %in% fia_data[["plot"]][["cn"]])
  )

  expect_true(
    all(fia_data[["subplot"]][["plt_cn"]] %in% fia_data[["plot"]][["cn"]])
  )

  expect_true(
    all(fia_data[["cond"]][["plt_cn"]] %in% fia_data[["plot"]][["cn"]])
  )

  expect_true(
    all(fia_data[["survey"]][["cn"]] %in% fia_data[["plot"]][["cn"]])
  )
})

test_that("tidy_fia produces expected output when postgis = FALSE", {
  aoi <- spData::us_states %>%
    dplyr::filter(NAME == "Connecticut") %>%
    sf::st_centroid() %>%
    sf::st_sf() %>%
    sf::st_transform(2163) %>%
    sf::st_buffer(20000)

  want_tables <- c("plot", "subplot", "cond", "survey", "tree")

  fia_data <- tidy_fia(
    aoi = aoi,
    postgis = FALSE,
    table_names = want_tables
  )

  expect_s3_class(
    fia_data,
    "tidyFIA"
  )

  expect_named(
    fia_data,
    expected = c(
      want_tables,
      "aoi",
      "states"
    ),
    ignore.order = TRUE
  )

  expect_true(
    nrow(fia_data[["tree"]]) > 0
  )

  expect_s3_class(
    fia_data[["plot"]],
    "sf"
  )

  expect_true(
    all(fia_data[["tree"]][["plt_cn"]] %in% fia_data[["plot"]][["cn"]])
  )

  expect_true(
    all(fia_data[["subplot"]][["plt_cn"]] %in% fia_data[["plot"]][["cn"]])
  )

  expect_true(
    all(fia_data[["cond"]][["plt_cn"]] %in% fia_data[["plot"]][["cn"]])
  )

  expect_true(
    all(fia_data[["survey"]][["cn"]] %in% fia_data[["plot"]][["cn"]])
  )
})

test_that("tidy_fia produces expected output when aoi is NULL and postgis = TRUE", {
  want_tables <- c("plot", "subplot", "cond", "survey", "tree")

  expect_error(
    tidy_fia(
      aoi = NULL,
      states = c("Rhode Island"),
      postgis = TRUE,
      table_names = want_tables
    )
  )

  fia_data <- tidy_fia(
    aoi = NULL,
    states = c("RI"),
    postgis = TRUE,
    table_names = want_tables
  )

  expect_s3_class(
    fia_data,
    "tidyFIA"
  )

  expect_named(
    fia_data,
    expected = c(
      want_tables,
      "aoi"
    ),
    ignore.order = TRUE
  )

  expect_true(
    nrow(fia_data[["tree"]]) > 0
  )

  expect_s3_class(
    fia_data[["plot"]],
    "sf"
  )

  expect_true(
    all(fia_data[["tree"]][["plt_cn"]] %in% fia_data[["plot"]][["cn"]])
  )

  expect_true(
    all(fia_data[["subplot"]][["plt_cn"]] %in% fia_data[["plot"]][["cn"]])
  )

  expect_true(
    all(fia_data[["cond"]][["plt_cn"]] %in% fia_data[["plot"]][["cn"]])
  )

  expect_true(
    all(fia_data[["survey"]][["cn"]] %in% fia_data[["plot"]][["cn"]])
  )
})

test_that("tidy_fia produces expected output when aoi is NULL and postgis = FALSE", {
  want_tables <- c("plot", "subplot", "cond", "survey", "tree")

  expect_error(
    tidy_fia(
      aoi = NULL,
      states = c("Carlifornia"),
      postgis = FALSE,
      table_names = want_tables
    )
  )

  fia_data <- tidy_fia(
    aoi = NULL,
    states = c("RI"),
    postgis = FALSE,
    table_names = want_tables
  )

  expect_s3_class(
    fia_data,
    "tidyFIA"
  )

  expect_named(
    fia_data,
    expected = c(
      want_tables,
      "aoi",
      "states"
    ),
    ignore.order = TRUE
  )

  expect_true(
    nrow(fia_data[["tree"]]) > 0
  )

  expect_s3_class(
    fia_data[["plot"]],
    "sf"
  )

  expect_true(
    all(fia_data[["tree"]][["plt_cn"]] %in% fia_data[["plot"]][["cn"]])
  )

  expect_true(
    all(fia_data[["subplot"]][["plt_cn"]] %in% fia_data[["plot"]][["cn"]])
  )

  expect_true(
    all(fia_data[["cond"]][["plt_cn"]] %in% fia_data[["plot"]][["cn"]])
  )

  expect_true(
    all(fia_data[["survey"]][["cn"]] %in% fia_data[["plot"]][["cn"]])
  )
})
