## code to prepare `ref_tables` dataset goes here
devtools::load_all()

forest_type <- read_ref_table(
  table_name = "REF_FOREST_TYPE"
)
species <- read_ref_table(
  table_name = "REF_SPECIES"
)

ref_tables <- list(
  forest_type = forest_type,
  species = species
)

usethis::use_data(ref_tables, overwrite = TRUE)
