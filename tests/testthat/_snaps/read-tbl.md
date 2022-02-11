# detects named inputs

    Problem with `read_tbl_*()` input `..1`.
    x Input `..1` is named.
    i This usually means that you've used `=` instead of `==`.
    i Did you mean `gene == "mdr1"`?

---

    Problem with `read_tbl_*()` input `..2`.
    x Input `..2` is named.
    i This usually means that you've used `=` instead of `==`.
    i Did you mean `chrom == "8"`?

# filter variable must exist

    Problem while computing `..1 = var == 5`..
    i Available objects are 'gene'.
    Caused by error in `dplyr::filter()`:
    ! Problem while computing `..1 = var == 5`.
    Caused by error in `var == 5`:
    ! comparison (1) is possible only for atomic and list types

# chrom and gene are deprecated

    Code
      read_tbl_ref_alt_cov(.tbl_ref = "small.csv", .tbl_alt = "small.csv", .tbl_cov = "small.csv",
        chrom = "13")
    Warning <lifecycle_warning_deprecated>
      The `chrom` argument of `read()` is deprecated as of miplicorn 0.1.0.
      Please use the `...` argument instead to filter data.
    Message <rlang_message>
      Joining, by = "gene_id"
      Joining, by = "gene_id"
      Joining, by = "gene_id"
    Output
      # A tibble: 0 x 10
      # ... with 10 variables: sample <chr>, gene_id <chr>, gene <chr>,
      #   mutation <chr>, func <chr>, aa_chng <chr>, target <chr>,
      #   ref_umi_count <dbl>, alt_umi_count <dbl>, coverage <dbl>

---

    Code
      read_tbl_ref_alt_cov(.tbl_ref = "small.csv", .tbl_alt = "small.csv", .tbl_cov = "small.csv",
        gene = "atp")
    Warning <lifecycle_warning_deprecated>
      The `gene` argument of `read()` is deprecated as of miplicorn 0.1.0.
      Please use the `...` argument instead to filter data.
    Message <rlang_message>
      Joining, by = "gene"
      Joining, by = "gene"
      Joining, by = "gene"
    Output
      # A tibble: 2 x 9
        sample gene  mutation   func     aa_chng target ref_umi_count alt_umi_count
        <chr>  <chr> <chr>      <chr>    <chr>   <chr>          <dbl>         <dbl>
      1 S1     atp6  atp6-A623E missense A623E   Yes                0             0
      2 S2     atp6  atp6-A623E missense A623E   Yes                0             0
      # ... with 1 more variable: coverage <dbl>

---

    Code
      read_tbl_ref_alt_cov(.tbl_ref = "small.csv", .tbl_alt = "small.csv", .tbl_cov = "small.csv",
        chrom = "13", gene = "atp")
    Warning <lifecycle_warning_deprecated>
      The `chrom` argument of `read()` is deprecated as of miplicorn 0.1.0.
      Please use the `...` argument instead to filter data.
      The `gene` argument of `read()` is deprecated as of miplicorn 0.1.0.
      Please use the `...` argument instead to filter data.
    Error <rlang_error>
      Multiple filtering criteria selected.
      x Cannot filter on both `chrom` and `gene`.
      i Select only one piece of information to filter on.

# read returns error if a file is empty

    Unable to read files.
    x "empty-file" is an empty file.

---

    Unable to read files.
    x "empty-file" is an empty file.

