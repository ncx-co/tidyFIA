context("PostGIS database queries work as expected")

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "fiadb",
  host = "fiadb.csrjp3emmira.us-east-1.rds.amazonaws.com",
  port = 5432,
  user = "tidyfia",
  password = Sys.getenv("TIDY_FIA_PASSWORD")
)

aoi <- data.frame(lat = 46.7867, lon = -92.1005) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  sf::st_transform(2163) %>%
  sf::st_buffer(10000) %>%
  sf::st_transform(4326)


test_that("query_plot_table works properly", {
  plot_table <- query_plot_table(aoi = aoi, con = con)
  class(plot_table)

  expect_s3_class(plot_table, "sf")
  expect_true("cn" %in% names(plot_table))
})

test_that("query_table works properly", {
  plt_cns <- c(
    "449073184489998", "449074607489998", "465276631489998",
    "372785550489998", "372787854489998", "372785549489998"
  )

  trees <- query_table(table_name = "tree", plt_cns = plt_cns, con = con)
  expect_s3_class(trees, "data.frame")
  expect_true(nrow(trees) > 0)
  expect_true(
    all(trees$plt_cn %in% plt_cns)
  )

  expect_error(
    query_table(table_name = "zzz", plt_cns = plt_cns, con = con)
  )
})

DBI::dbDisconnect(con)
