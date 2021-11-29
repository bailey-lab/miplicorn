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

data <- read_tbl_reference(file, gene == "atp6")
data
#> # A tibble: 260 × 8
#>   sample     gene_id       gene  mutation_name  exonic_func   aa_change targeted
#>   <chr>      <chr>         <chr> <chr>          <chr>         <chr>     <chr>
#> 1 D10-JJJ-23 PF3D7_0106300 atp6  atp6-Ala623Glu missense_var… Ala623Glu Yes
#> 2 D10-JJJ-43 PF3D7_0106300 atp6  atp6-Ala623Glu missense_var… Ala623Glu Yes
#> 3 D10-JJJ-55 PF3D7_0106300 atp6  atp6-Ala623Glu missense_var… Ala623Glu Yes
#> 4 D10-JJJ-5  PF3D7_0106300 atp6  atp6-Ala623Glu missense_var… Ala623Glu Yes
#> 5 D10-JJJ-47 PF3D7_0106300 atp6  atp6-Ala623Glu missense_var… Ala623Glu Yes
#> # … with 255 more rows, and 1 more variable: ref_umi_count <dbl>

arrange_natural(data, sample, targeted)
#> # A tibble: 260 × 8
#>   sample    gene_id       gene  mutation_name  exonic_func    aa_change targeted
#>   <fct>     <chr>         <chr> <chr>          <chr>          <chr>     <fct>
#> 1 D10-JJJ-1 PF3D7_0106300 atp6  atp6-Gly639Asp missense_vari… Gly639Asp No
#> 2 D10-JJJ-1 PF3D7_0106300 atp6  atp6-Ser466Asn missense_vari… Ser466Asn No
#> 3 D10-JJJ-1 PF3D7_0106300 atp6  atp6-Ala623Glu missense_vari… Ala623Glu Yes
#> 4 D10-JJJ-1 PF3D7_0106300 atp6  atp6-Glu431Lys missense_vari… Glu431Lys Yes
#> 5 D10-JJJ-1 PF3D7_0106300 atp6  atp6-Ser769Asn missense_vari… Ser769Asn Yes
#> # … with 255 more rows, and 1 more variable: ref_umi_count <dbl>
```
