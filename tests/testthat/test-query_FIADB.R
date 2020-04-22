context("PostGIS database queries work as expected")

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "fiadb",
  host = "fiadb.csrjp3emmira.us-east-1.rds.amazonaws.com",
  port = 5432,
  user = "tidyfia",
  password = Sys.getenv("TIDY_FIA_PASSWORD")
)
