# tidyFIA

## Install
At this time you must install the `tidyFIA` package via GitHub.
```r
devtools::install_github("SilviaTerra/tidyFIA")
```

## Usage
The main function is `tidy_fia` which will download and import the specified tables from FIADB that correspond to an area of interest represented by a geospatial object in R (class `sf`) or a list of state abbreviations (e.g. `c("CA", "NV")`).

To get started you can simply run this example command to get the FIA tables for the state of Minnesota:
```r
tidyFIA::tidy_fia(
  states = "MN"
)
```
