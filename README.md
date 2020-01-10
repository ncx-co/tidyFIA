# tidyFIA <a href='https://silviaterra.com'><img src='man/figures/logo.png' align="right" height="139" /></a>

## Install
At this time you must install the `tidyFIA` package via GitHub.
```r
devtools::install_github("SilviaTerra/tidyFIA")
```

## Usage
The main function is `tidy_fia` which will download and import the specified tables from FIADB that correspond to an area of interest represented by a geospatial object in R (class `sf`) or a list of state abbreviations (e.g. `c("CA", "NV")`).

To get started you can simply run this example command to get the FIA tables for the state of Minnesota:
```r
mn_data <- tidyFIA::tidy_fia(
  states = "MN"
)
```

The example will download these tables by default:
`"PLOT", "SUBPLOT", "COND", "TREE", "SURVEY"`

Each table is an element in the list `mn_data`.
To use one of the tables you call them like this:
```r
mn_data[["PLOT"]]
mn_data[["TREE"]]
...
```
