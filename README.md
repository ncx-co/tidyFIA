# tidyFIA <img src='man/figures/logo.png' align="right" height="139" /></a>

## Install
At this time you must install the `tidyFIA` package via GitHub.
```r
remotes::install_github("ncx-co/tidyFIA")
```

## Usage
The main function is `tidy_fia` which will import the specified tables from
FIADB that correspond to an area of interest represented by a geospatial object
in R (class `sf`) or a list of state abbreviations (e.g. `c("CA", "NV")`).

To get started you can simply run this example command to get the FIA tables for
the state of Minnesota.
```r
mn_data <- tidyFIA::tidy_fia(
  states = "MN",
  postgis = FALSE,
  file_dir = tempdir()  # replace this with any normal directory to cache downloaded files!
)
```

By default, state-wise zip files will be downloaded from the FIA datamartto the
R temporary directory(`tempdir()`) but you can specify them to be downloaded 
to any directory which makes it possible to use files downloaded during a previous
R session.

## Database
[NCX](https://ncx.com) is hosting a clone of FIADB data in a PostGIS database
and you are welcome to use it!
The code NCX used to build the FIADB clone is located in the [`fiadb`](fiadb/)
subdirectory of this repository.
Please reach out to [Henry Rodman](henry@ncx.com) if you would like the password
for the database and he will gladly share it.

Alternatively you can ask `tidy_fia` to download and import the data directly
from the CSVs stored at the
[FIA datamart](https://apps.fs.usda.gov/fia/datamart/datamart.html) by setting
the parameter `postgis` to `FALSE`.
This is slower than querying the database but will work without any
authentication.

The example will import these tables by default:
`"plot", "subplot", "cond", "tree", "survey"`

Each table is an element in the list `mn_data`.
To use one of the tables you call them like this:
```r
mn_data[["plot"]]
mn_data[["tree"]]
...
```
