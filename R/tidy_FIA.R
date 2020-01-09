#' @title Create tidy FIA tables
#' @description This function queries the FIA database, by state abbreviation(s)
#' or area of interest,and returns a list of tidy data objects including the
#' TREE, PLOT, COND, and SURVEY tables.
#' @param states a character vector of state abbreviations, ignored if `aoi` is
#' supplied.
#' @param aoi sf object containing area of interest
#' @param files list of FIA tables to download. Tables must be identified by
#' their 'Oracle Table Name' as described in the 'Index of Tables' in FIADB
#' User Guide.
#' @return a list object containing tidy data
#' @author Henry Rodman, Brian Clough
#' @importFrom magrittr %>%
#' @export

tidy_fia <- function(states = NULL, aoi = NULL,
                     files = c("PLOT", "SUBPLOT", "COND", "TREE", "SURVEY")) {
  if (is.null(aoi) & is.null(states)) {
    stop("please specify an AOI or a list of US states")
  }

  if (!is.null(aoi)) {
    states <- spData::us_states %>%
      sf::st_transform(sf::st_crs(aoi)) %>%
      sf::st_intersection(aoi) %>%
      dplyr::mutate(
        ABB = state.abb[match(NAME, state.name)]
      ) %>%
      dplyr::pull(ABB)
  }

  # download tables
  fia_db_files <- purrr::map(
    .x = states,
    .f = ~ download_by_state(state = .x)
  )

  # combine tables
  tables <- purrr::map(
    .x = files,
    .f = ~ stack_tables(
      table_name = .x,
      fia_db_files = fia_db_files
    )
  )
  names(tables) <- files

  # uniquely identify plots by location
  tables[["plot_locs"]] <- tables[["PLOT"]] %>%
    dplyr::select(
      STATECD, UNITCD, COUNTYCD, PLOT,
      LAT, LON, ELEV
    ) %>%
    dplyr::distinct() %>%
    tidyr::unite(
      "plot_loc_id",
      c("STATECD", "UNITCD", "COUNTYCD", "PLOT"),
      sep = "_"
    ) %>%
    sf::st_as_sf(
      coords = c("LON", "LAT"),
      crs = 4326,
      remove = FALSE
    )

  tables[["PLOT"]] <- tables[["PLOT"]] %>%
    tidyr::unite(
      "plot_loc_id",
      c("STATECD", "UNITCD", "COUNTYCD", "PLOT"),
      sep = "_",
      remove = FALSE
    )

  # clip to aoi if applicable
  if (!is.null(aoi)) {
    message("filtering plot locations down to aoi")
    tables[["plot_locs"]] <- tables[["plot_locs"]] %>%
      sf::st_intersection(sf::st_transform(aoi, 4326))

    tables[["PLOT"]] <- tables[["PLOT"]] %>%
      dplyr::filter(plot_loc_id %in% tables[["plot_locs"]][["plot_loc_id"]])

    # filter all other tables to CNs in geographically filtered plots
    for (file in files) {
      if ("PLT_CN" %in% names(tables[[file]])) {
        tables[[file]] <- tables[[file]] %>%
          dplyr::filter(PLT_CN %in% tables[["PLOT"]][["CN"]])
      } else {
        tables[[file]] <- tables[[file]] %>%
          dplyr::filter(CN %in% tables[["PLOT"]][["CN"]])
      }
    }
  }

  # append aoi
  if (is.null(aoi)) {
    aoi <- spData::us_states %>%
      dplyr::mutate(
        ABB = state.abb[match(NAME, state.name)]
      ) %>%
      dplyr::filter(ABB %in% states)
  }

  tables[["aoi"]] <- aoi

  # append states
  tables[["states"]] <- states

  class(tables) <- c("tidyFIA", class(tables))

  return(tables)
}

#' @title Stack tables
#' @description Import all files called `table_name` from `fia_db_files` as
#' one dataframe
#'
#' @param table_name Oracle table name from FIADB
#' @param fia_db_files list of files downloaded by state
#'
#' @return dataframe containing data comibined from all states

stack_tables <- function(table_name, fia_db_files) {
  purrr::map(fia_db_files, table_name) %>%
    vroom::vroom(
      delim = ",",
      col_types = vroom::cols(
        .default = "?",
        CN = "c",
        PLT_CN = "c",
        PREV_PLT_CN = "c",
        SRV_CN = "c",
        CTY_CN = "c"
      )
    )
}


#' @title Read FIA reference table
#'
#' @param table_name name of reference table e.g. "REF_SPECIES"
#'
#' @return dataframe of reference table
#' @export

read_ref_table <- function(table) {
  url <- glue::glue(
    "https://apps.fs.usda.gov/fia/datamart/CSV/{table}.csv"
  )
  vroom::vroom(url, delim = ",")
}

#' @title Plot method for `tidyFIA` class
#' @author Henry Rodman
#' @param x object of class `tidyFIA` (output from `tidy_fia`)
#' @param ... Arguments to be passed `ggplot`
#' @method plot tidyFIA
#' @import ggplot2
#' @export

plot.tidyFIA <- function(x, ...) {
  ggplot(...) +
    geom_sf(
      data = x[["aoi"]],
      size = 0.5,
      color = "black",
      alpha = 0
    ) +
    geom_sf(
      data = x[["plot_locs"]],
      color = "black",
      alpha = 1
    ) +
    theme_bw() +
    coord_sf() +
    ggtitle("FIA plot distribution")
}
