# tidyFIA <a href='https://silviaterra.com'><img src='man/figures/logo.png' align="right" height="139" /></a>

## Install
At this time you must install the `tidyFIA` package via GitHub.
```r
remotes::install_github("SilviaTerra/tidyFIA")
```

## Usage
The main function is `tidy_fia` which will import the specified tables from FIADB that correspond to an area of interest represented by a geospatial object in R (class `sf`) or a list of state abbreviations (e.g. `c("CA", "NV")`).

To get started you can simply run this example command to get the FIA tables for the state of Minnesota:
```r
mn_data <- tidyFIA::tidy_fia(
  states = "MN",
  postgis = TRUE
)
```
SilviaTerra is hosting the FIADB data in a PostGIS database. Please reach out to [Henry Rodman](henry@silviaterra.com) if you would like the password for the database. Alternatively you can ask `tidy_fia` to download and import the data directly from the CSVs stored at the [FIA datamart](https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html) by setting the parameter `postgis` to `FALSE`. This is slower than querying the database but will work without any authentication.

The example will import these tables by default:
`"plot", "subplot", "cond", "tree", "survey"`

Each table is an element in the list `mn_data`.
To use one of the tables you call them like this:
```r
mn_data[["plot"]]
mn_data[["tree"]]
...
```
