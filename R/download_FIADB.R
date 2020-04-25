#' @title download FIA database files by state
#' @description This function downloads FIA data for a single state
#'
#' @param state character state abbreviation code
#' @param file_dir directory into which files will be downloaded. By default
#' this is the R temporary directory (`tempdir()`)
#' @param files list of FIA tables to download. Tables must be identified by
#' their 'Oracle Table Name' as described in the 'Index of Tables' in FIADB
#' User Guide.
#'
#' @return list of filepaths
#' @author Henry Rodman
#' @export
#'
#' @examples
#' \dontrun{
#' fia_files <- download_fia_by_state("MN")
#' }

download_by_state <- function(state, file_dir = tempdir(),
                              files = c("PLOT", "SUBPLOT", "COND", "TREE", "SURVEY")) {

  urls <- glue::glue(
    "https://apps.fs.usda.gov/fia/datamart/CSV/{state}_{files}.zip"
  )
  local_files <- glue::glue("{file_dir}/{state}_{files}.csv")

  downloaded_files <- purrr::map(
    .x = urls,
    .f = ~ download_and_unzip(url = .x, file_dir = file_dir)
  )

  names(downloaded_files) <- files

  return(downloaded_files)
}

#' @title download and unzip file from web
#' @description download a file from a web address (`url`) and unzip it into a
#' specified directory (`file_dir`).
#'
#' @param url web address of .zip file to be downloaded
#' @param file_dir file path of directory into which file will be unzipped
#'
#' @return list of file paths extracted from .zip
#'
#' @examples
#' \dontrun{
#' unzipped <- download_and_unzip(
#'   url = "https://apps.fs.usda.gov/fia/datamart/CSV/MN_PLOT.zip",
#'   file_dir = tempdir()
#' )
#' }

download_and_unzip <- function(url, file_dir) {
  message(
    glue::glue("downloading {basename(url)}")
  )

  zip_file <- file.path(
    file_dir,
    basename(url)
  )

  if (!file.exists(zip_file)) {
    downloaded <- try(utils::download.file(url, destfile = zip_file))
  }

  if ("try-error" %in% class(downloaded)) {
    stop(
      glue::glue(
        "{url} is not available to download at the moment"
      )
    )
  }

  utils::unzip(
    zipfile = zip_file,
    exdir = file_dir
  )
}
