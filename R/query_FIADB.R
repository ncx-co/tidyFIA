#' @title query_plot_table
#' @description Perform spatial query on the plot table
#'
#' @param aoi character state abbreviation code
#' @param con database connection
#' @return sf object with FIA plot records
#' @author Henry Rodman

query_plot_table <- function(aoi, con) {
  outline <- aoi |>
    dplyr::mutate(aoi = "aoi") |>
    dplyr::group_by(.data[["aoi"]]) |>
    dplyr::summarize() |>
    sf::st_geometry() |>
    sf::st_as_text()

  plot_query <- glue::glue(
    "SELECT * FROM public.plot WHERE ST_Contains(
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

query_table <- function(table_name, plt_cns, con) {
  message(
    glue::glue("finding matching records in {table_name} table")
  )

  tab <- dplyr::tbl(
    con,
    dbplyr::in_schema(dbplyr::sql("public"), dbplyr::sql(table_name))
  )

  if ("plt_cn" %in% colnames(tab)) {
    tab <- dplyr::filter(
      tab,
      .data$plt_cn %in% !!plt_cns
    )
  }

  dplyr::collect(tab)
}
