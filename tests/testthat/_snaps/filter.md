# error if column does not exist

    Data needs the column `gene`.

# filter_*() functions advise user to use dplyr::filter()

    Code
      filter_coverage(tibble::tibble(coverage = c(2, 3)), 2)
    Warning <rlang_warning>
      This function provides a simple filtering interface.
      i For more complex filtering, please use `dplyr::filter()`.
      This warning is displayed once every 8 hours.
    Output
      # A tibble: 2 x 1
        coverage
           <dbl>
      1        2
      2        3

