# tabuleia

> tabulEIA Automatize the process of build species occurrence list for EIA reports.

## Disclaimer

This software is preliminary or provisional. No warranty, expressed or
implied, is made by the maintainer. Software releases constitute no
warranty. The software is provided *as is* on the condition that the
maintainer shall not be held liable or responsible for any damages
resulting from the authorized or unauthorized use of the software.

## What is tabulEIA?

TabulEIA provide species list tables for a region of interest by merging data from different sources pre-compiled to the 10 km UTM grid cells.
- Field inventories
- Atlases
- Flora-on database
- eBird
- iNaturalist (research grade)
- Natura 2000 report

Data compiled at the 10 km UTM grid will be  The Provided region of interest will be  
## Installation

You can install the released version of tabulEIA from
[Github](https:://github.com/pecard/tabuleia) with:

``` r
library(devtools)
install_github("pecard/tabuleia")
```

## Contribute

You are welcome to contribute to the package development and improvement
by [filling an issue](https://github.com/pecard/tabuleia/issues) at the
package web page.

## Usage

Run a basic example:

``` r
library(tabulEIA)
library(tidyverse)
library(sf)
library(sp)
library(tabulEIA)
library(geotidy)

# Copy geometry directly from QGIS wkt
pol <- st_geomfromtext('Polygon ((-8.2522 37.8960, -8.2513 37.8960, -8.2513 37.8951,
                       -8.2522 37.8951, -8.2522 37.8960))')
pol <- st_as_sf(pol) %>% st_set_crs(4326)
utm_all = utm_id(grid = utm10k, roi = pol, buff = NULL, contiguity = 'queen')

# Flora ----
# inventories from field work
dados_campo <- read.delim('~path_to_my_field_data/fielddata.txt')

tflora <- tabulEIA::tabulFlora(fielddata = dados_campo, utm_ae = utm_all$ae, utm_q = utm_all$contig,
                               biblio = 'all')

```
