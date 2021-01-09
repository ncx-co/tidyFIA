#' @title query_plot_table
#' @description Perform spatial query on the plot table
#'
#' @param aoi character state abbreviation code
#' @param con database connection
#' @return sf object with FIA plot records
#' @author Henry Rodman

query_plot_table <- function(aoi, con) {
  outline <- aoi %>%
    dplyr::mutate(aoi = "aoi") %>%
    dplyr::group_by(.data[["aoi"]]) %>%
    dplyr::summarize(.groups = "drop") %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_geometry() %>%
    sf::st_as_text()

  plot_query <- glue::glue(
    "SELECT * FROM fs_fiadb.plot WHERE ST_Contains(
      ST_GeometryFromText('{outline}', 4326),
      plot.geom::geometry
    );"
  )

  plot_table <- sf::st_read(con, query = plot_query)

  return(plot_table)
}

#' @title query_table
#'
#' @param table_name 'Oracle Table Name' as described in the 'Index of Tables' in FIADB
#' User Guide.
#' @param plt_cns character vector of FIA plot CNs from which to pull records
#' @param con connection to fiadb database
#'
#' @return dataframe with matching records from the target table

query_table <- function(table_name, con) {
  
  table_call <- as.character(glue::glue("fs_fiadb.{table_name}"))
  
  dplyr::tbl(
    con,
    dbplyr::in_schema("fiadb", table_call)
  )
  
}

#' stratum_joins
#'
#' Execute FIA DB stratum joins 
#' @param con database connection
#' @param eval_typ character; type of evaluation desired
#' @return object of class /code{tbl_PqConnection} linking joined tables
#'   # Fk: https://bit.ly/2XrnBhm
#' @author Nathan Rutenbeck

stratum_joins <- function(con, eval_typ = NULL) {
  # Follows from source: https://bit.ly/2XrnBhm

  tbl_names <- c(
    "pop_eval_grp",
    "pop_eval_typ",
    "pop_eval",
    "pop_stratum",
    "pop_estn_unit"
  )

  for (t in tbl_names) {
    assign(t, query_table(con = con, t))
  }
  
  # do table joins (see Figure 3-1 in source)
  pop_eval_typ %>%
    # Filter to evaluation type
    dplyr::filter(eval_typ == !!eval_typ) %>%
    dplyr::select(
      -.data[["cn"]],
      -tidyselect::contains("created"),
      -tidyselect::contains("modified"),
      -tidyselect::contains("notes")
    ) %>%
    # Join to EVAL_GRP
    dplyr::left_join(
      dplyr::select(
        pop_eval_grp,
        -tidyselect::contains("created"),
        -tidyselect::contains("modified"),
        -tidyselect::contains("notes")
      ),
      by = c("eval_grp_cn" = "cn")
    ) %>%
    ### Join to POP_EVAL table ###
    dplyr::left_join(
      dplyr::select(
        pop_eval,
        -tidyselect::contains("created"),
        -tidyselect::contains("modified"),
        -tidyselect::contains("notes")
      ),
      by = c("eval_cn" = "cn", "eval_grp_cn", "rscd", "statecd")
    ) %>%
    ### Join to POP_ESTN_UNIT table ###
    dplyr::left_join(
      dplyr::select(
        pop_estn_unit,
        tidyselect::everything(),
        estn_unit_cn = .data[["cn"]],
        -tidyselect::contains("created"),
        -tidyselect::contains("modified"),
        -tidyselect::contains("notes")
      ),
      by = c("eval_cn", "rscd", "statecd", "evalid")
    ) %>%
    ### Join to POP_STRATUM table ###
    dplyr::left_join(
      dplyr::select(
        pop_stratum,
        tidyselect::everything(),
        stratum_cn = .data[["cn"]],
        -tidyselect::contains("created"),
        -tidyselect::contains("modified"),
        -tidyselect::contains("notes")
      ),
      by = c("rscd", "statecd", "evalid", "estn_unit_cn", "estn_unit")
    )
}

#' plot_joins
#'
#' Execute FIA DB stratum joins 
#' @param con database connection
#' @param eval_typ character; type of evaluation desired
#' @return object of class /code{tbl_PqConnection} linking joined tables
#'   # Fk: https://bit.ly/2XrnBhm
#' @author Nathan Rutenbeck

plot_joins <- function(con, plt_cns) {
    
  for (t in c("pop_plot_stratum_assgn", "cond", "plot", "tree")) {
    assign(t, query_table(con = con, t))
  }
  
  pop_plot_stratum_assgn %>%
    # Filter to area of interest
    dplyr::filter(plt_cn %in% dplyr::local(plt_cns)) %>%
    dplyr::select(
      tidyselect::everything(),
      -.data[["cn"]],
      -tidyselect::contains("created"),
      -tidyselect::contains("modified"),
      -tidyselect::contains("notes")
    ) %>%
    # Join to PLOT table
    left_join(
      dplyr::select(
        plot,
        tidyselect::everything(),
        plt_cn = .data[["cn"]],
        -tidyselect::contains("created"),
        -tidyselect::contains("modified"),
        -tidyselect::contains("notes")
      ),
      by = c("plt_cn", "statecd", "invyr", "unitcd", "countycd", "plot")
    ) %>%
    # Join to COND table
    left_join(
      dplyr::select(
        cond,
        tidyselect::everything(),
        -.data[["cn"]],
        -tidyselect::contains("created"),
        -tidyselect::contains("modified"),
        -tidyselect::contains("notes")
      ),
      by = c(
        "plt_cn", "statecd", "invyr", "unitcd", "countycd", 
        "plot", "cycle", "subcycle"
      )
    ) %>%
    left_join(
      dplyr::select(
        tree,
        tidyselect::everything(),
        tree_cn = .data[["cn"]],
        -tidyselect::contains("created"),
        -tidyselect::contains("modified"),
        -tidyselect::contains("notes")
      ),
      by = c(
        "plt_cn", "statecd", "invyr", "unitcd", "countycd",
        "plot", "cycle", "subcycle", "condid"
      )
    )
}
