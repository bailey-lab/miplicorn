<!-- README.md is generated from README.Rmd. Please edit that file -->

# miplicorn <a href='https://bailey-lab.github.io/miplicorn'><img src='man/figures/logo.png' align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/bailey-lab/miplicorn/workflows/R-CMD-check/badge.svg)](https://github.com/bailey-lab/miplicorn/actions)
[![Codecov test
coverage](https://codecov.io/gh/bailey-lab/miplicorn/branch/master/graph/badge.svg)](https://codecov.io/gh/bailey-lab/miplicorn?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

<!-- badges: end -->

miplicorn establishes a unified framework for molecular inversion probe
(MIP) and amplicon analysis. It provides the tools to quickly parse
files several gigabytes large, filter and manipulate data, analyze data,
and, most informatively, visualize data.

## Installation

You may install the package from
[Github](https://github.com/bailey-lab/miplicorn) using `devtools`.

```r
# install.packages("devtools")
devtools::install_github(repo = "https://github.com/bailey-lab/miplicorn")
```

## Usage

See `vignette("miplicorn")` for a more extensive introduction and a
demonstration of several features of the package.

```r
library(miplicorn)

file <- miplicorn_example("reference_AA_table.csv")

data <- read_file(file, gene == "atp6")
data
#> # A tibble: 260 × 8
#>   sample     gene_id   gene  mutation_name exonic_func  aa_change targeted value
#>   <chr>      <chr>     <chr> <chr>         <chr>        <chr>     <chr>    <dbl>
#> 1 d10_jjj_23 PF3D7_01… atp6  atp6-Ala623G… missense_va… Ala623Glu Yes        608
#> 2 d10_jjj_43 PF3D7_01… atp6  atp6-Ala623G… missense_va… Ala623Glu Yes         20
#> 3 d10_jjj_55 PF3D7_01… atp6  atp6-Ala623G… missense_va… Ala623Glu Yes        158
#> 4 d10_jjj_5  PF3D7_01… atp6  atp6-Ala623G… missense_va… Ala623Glu Yes          2
#> 5 d10_jjj_47 PF3D7_01… atp6  atp6-Ala623G… missense_va… Ala623Glu Yes          1
#> # … with 255 more rows

arrange_natural(data, sample, targeted)
#> # A tibble: 260 × 8
#>   sample    gene_id   gene  mutation_name  exonic_func  aa_change targeted value
#>   <fct>     <chr>     <chr> <chr>          <chr>        <chr>     <fct>    <dbl>
#> 1 d10_jjj_1 PF3D7_01… atp6  atp6-Gly639Asp missense_va… Gly639Asp No          10
#> 2 d10_jjj_1 PF3D7_01… atp6  atp6-Ser466Asn missense_va… Ser466Asn No           2
#> 3 d10_jjj_1 PF3D7_01… atp6  atp6-Ala623Glu missense_va… Ala623Glu Yes         10
#> 4 d10_jjj_1 PF3D7_01… atp6  atp6-Glu431Lys missense_va… Glu431Lys Yes          5
#> 5 d10_jjj_1 PF3D7_01… atp6  atp6-Ser769Asn missense_va… Ser769Asn Yes          1
#> # … with 255 more rows
```
