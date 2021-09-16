
<!-- README.md is generated from README.Rmd. Please edit that file -->

# miplicorn

<!-- badges: start -->

[![R-CMD-check](https://github.com/bailey-lab/miplicorn/workflows/R-CMD-check/badge.svg)](https://github.com/bailey-lab/miplicorn/actions)
[![Codecov test
coverage](https://codecov.io/gh/bailey-lab/miplicorn/branch/master/graph/badge.svg)](https://codecov.io/gh/bailey-lab/miplicorn?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Installation

To install the package, please follow the code below. In order to
install, `devtools` must be installed. Note that as this is still a
private repo, you must enter an authorization token to
`devtools::install_github`. Follow the instructions at [Managing
Git(Hub)
Credentials](https://usethis.r-lib.org/articles/articles/git-credentials.html)
to generate a personal access token (PAT) for github.

``` r
# install.packages("devtools")
devtools::install_github(repo = "https://github.com/bailey-lab/miplicorn",
                         auth_token = "PAT token")
```
