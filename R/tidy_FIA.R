#' @title Create tidy FIA tables
#' @description This function queries the FIA database, by state abbreviation(s)
#' or area of interest,and returns a list of tidy data objects including the
#' TREE, PLOT, COND, and SURVEY tables.
#' @param states a character vector of state abbreviations, ignored if \code{aoi} is
#' supplied.
#' @param aoi sf object containing area of interest
#' @param table_names list of FIA tables to download. Tables must be identified by
#' their 'Oracle Table Name' as described in the 'Index of Tables' in FIADB
#' User Guide.
#' @param postgis logical if true, query PostGIS database instead of downloading
#' csvs from FIA Datamart.
#' @return a list object containing tidy data
#' @author Henry Rodman, Brian Clough
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

tidy_fia <- function(states = NULL, aoi = NULL, postgis = TRUE,
                     table_names = c("plot", "subplot", "cond", "tree", "survey")) {
  if (is.null(aoi) & is.null(states)) {
    stop("please specify an AOI or a list of US states")
  }

  aoi <- sf::st_transform(aoi, crs = 4326)

  if (is.null(aoi) & !is.null(states)) {
    aoi <- spData::us_states %>%
      dplyr::mutate(
        ABB = datasets::state.abb[match(.data[["NAME"]], datasets::state.name)]
      ) %>%
      dplyr::filter(.data[["ABB"]] %in% states)
  }

  if (postgis) {
    if (nchar(Sys.getenv("TIDY_FIA_PASSWORD")) == 0) {
      stop(
        "you must add TIDY_FIA_PASSWORD in your .Renviron file"
      )
    }
    # connect to database
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = "fiadb",
      host = "fiadb.csrjp3emmira.us-east-1.rds.amazonaws.com",
      port = 5432,
      user = "tidyfia",
      password = Sys.getenv("TIDY_FIA_PASSWORD")
    )
    message("connected to tidyfia database")

    # identify plot CNs
    plot_table <- query_plot_table(aoi = aoi, con = con)

    # retrieve rest of tables
    table_names <- setdiff(tolower(table_names), "plot")
    tables <- purrr::map(
      .x = table_names,
      .f = ~ query_table(
        table_name = .x,
        plt_cns = plot_table$cn,
        con = con
      )
    )

    names(tables) <- table_names

    DBI::dbDisconnect(con)

    # append plot table
    tables[["plot"]] <- plot_table

  } else {
    if (!is.null(aoi)) {
      states <- spData::us_states %>%
        sf::st_transform(sf::st_crs(aoi)) %>%
        sf::st_intersection(aoi) %>%
        dplyr::mutate(
          ABB = datasets::state.abb[match(.data[["NAME"]], datasets::state.name)]
        ) %>%
        dplyr::pull(.data[["ABB"]])
    }

    # download tables
    fia_db_files <- purrr::map(
      .x = states,
      .f = ~ download_by_state(state = .x, files = table_names)
    )

      # combine tables
    tables <- purrr::map(
      .x = table_names,
      .f = ~ stack_tables(
        table_name = .x,
        fia_db_files = fia_db_files
      )
    )
    names(tables) <- tolower(table_names)

    # spatialize plots table
    tables[["plot"]] <- tables[["plot"]] %>%
      sf::st_as_sf(
        coords = c("lon", "lat"),
        crs = 4326,
        remove = FALSE
      )

    # clip to aoi if applicable
    if (!is.null(aoi)) {
      message("filtering plot locations down to aoi")
      tables[["plot"]] <- tables[["plot"]] %>%
        sf::st_intersection(sf::st_transform(aoi, 4326))

      # filter all other tables to CNs in geographically filtered plots
      for (file in table_names) {
        if ("plt_cn" %in% names(tables[[file]])) {
          tables[[file]] <- tables[[file]] %>%
            dplyr::filter(
              .data[["plt_cn"]] %in% tables[["plot"]][["cn"]]
            )
        } else {
          tables[[file]] <- tables[[file]] %>%
            dplyr::filter(
              .data[["cn"]] %in% tables[["plot"]][["cn"]]
            )
        }
      }
    }

    # append aoi
    if (is.null(aoi)) {
      aoi <- spData::us_states %>%
        dplyr::mutate(
          state_abb = datasets::state.abb[match(.data[["NAME"]], datasets::state.name)]
        ) %>%
        dplyr::filter(.data[["state_abb"]] %in% states)
    }

    tables[["states"]] <- states
  }

  # append aoi
  tables[["aoi"]] <- aoi

  # export
  class(tables) <- c("tidyFIA", class(tables))

  return(tables)
}

#' @title Stack tables
#' @description Import all files called \code{table_name} from \code{fia_db_files} as
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
    ) %>%
    dplyr::rename_all(
      tolower
    )
}

#' @title Read FIA reference table
#'
#' @param table_name name of reference table e.g. "REF_SPECIES"
#'
#' @return dataframe of reference table
#' @export

read_ref_table <- function(table_name) {
  url <- glue::glue(
    "https://apps.fs.usda.gov/fia/datamart/CSV/{table_name}.csv"
  )
  vroom::vroom(url, delim = ",") %>%
    dplyr::rename_all(
      tolower
    )
}

#' @title Plot method for \code{tidyFIA} class
#' @author Henry Rodman
#' @param x object of class \code{tidyFIA} (output from \code{\link{tidy_fia}})
#' @param ... Arguments to be passed \code{ggplot}
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
      data = x[["plot"]],
      color = "black",
      alpha = 1
    ) +
    theme_bw() +
    coord_sf() +
    ggtitle("FIA plot distribution")
}
