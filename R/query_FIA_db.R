#' @title download FIA database files by state
#' @description This function downloads FIA data for a single state
#' @param stateAbb character state abbreviation code
#' @return a list of file paths for the FIA data
#' @author Henry Rodman
#' @examples
#' \dontrun{
#' fiaFiles <- download_fia_by_state("MN")
#' }
#' @export

download_fia_by_state <- function(stateAbb) {
  # download PLOT table
  message("downloading PLOT table")
  plotUrl <- paste0("https://apps.fs.usda.gov/fia/datamart/CSV/", stateAbb, "_PLOT.zip")

  plotZip <- paste0("/tmp/", stateAbb, "_PLOT.zip")
  plotFile <- paste0("/tmp/", stateAbb, "_PLOT.csv")
  if (!file.exists(plotFile)) {
    download.file(plotUrl, destfile = plotZip)
    system(paste0("cd /tmp; unzip ", stateAbb, "_PLOT.zip"))
  }

  # download SUBPLOT table
  message("downloading SUBPLOT table")
  subplotUrl <- paste0("https://apps.fs.usda.gov/fia/datamart/CSV/", stateAbb, "_SUBPLOT.zip")

  subplotZip <- paste0("/tmp/", stateAbb, "_SUBPLOT.zip")
  subplotFile <- paste0("/tmp/", stateAbb, "_SUBPLOT.csv")
  if (!file.exists(subplotFile)) {
    download.file(subplotUrl, destfile = subplotZip)
    system(paste0("cd /tmp; unzip ", stateAbb, "_SUBPLOT.zip"))
  }

  # download TREE table
  message("downloading TREE table")
  treeUrl <- paste0("https://apps.fs.usda.gov/fia/datamart/CSV/", stateAbb, "_TREE.zip")
  treeZip <- paste0("/tmp/", stateAbb, "_TREE.zip")
  treeFile <- paste0("/tmp/", stateAbb, "_TREE.csv")
  if (!file.exists(treeFile)) {
    download.file(treeUrl, destfile = treeZip)
    system(paste0("cd /tmp; unzip ", stateAbb, "_TREE.zip"))
  }

  # download COND table
  message("downloading COND table")
  condUrl <- paste0("https://apps.fs.usda.gov/fia/datamart/CSV/", stateAbb, "_COND.zip")
  condZip <- paste0("/tmp/", stateAbb, "_COND.zip")
  condFile <- paste0("/tmp/", stateAbb, "_COND.csv")
  if (!file.exists(condFile)) {
    download.file(condUrl, destfile = condZip)
    system(paste0("cd /tmp; unzip ", stateAbb, "_COND.zip"))
  }

  # download SURVEY table
  message("downloading SURVEY table")
  surveyUrl <- paste0("https://apps.fs.usda.gov/fia/datamart/CSV/", stateAbb, "_SURVEY.zip")
  surveyZip <- paste0("/tmp/", stateAbb, "_SURVEY.zip")
  surveyFile <- paste0("/tmp/", stateAbb, "_SURVEY.csv")
  if (!file.exists(surveyFile)) {
    download.file(surveyUrl, destfile = surveyZip)
    system(paste0("cd /tmp; unzip ", stateAbb, "_SURVEY.zip"))
  }

  outFiles <- list(
    plotFile = plotFile,
    subplotFile = subplotFile,
    treeFile = treeFile,
    condFile = condFile,
    surveyFile = surveyFile
  )
  return(outFiles)
}

