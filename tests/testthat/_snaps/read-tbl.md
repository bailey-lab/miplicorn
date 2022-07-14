# detects named inputs

    Input `gene` is named.
    i This usually means that you've used `=` instead of `==`.
    i Did you mean `gene == "mdr1"`?

---

    Input `gene` is named.
    i This usually means that you've used `=` instead of `==`.
    i Did you mean `gene == "g1"`?

---

    Input `var` is named.
    i This usually means that you've used `=` instead of `==`.
    i Did you mean `var == "g1"`?

# filter variable must exist

    Problem while computing `..1 = var == 5`..
    i Filter using the columns 'gene_id', 'gene', 'mutation', 'func', 'aa_chng', and 'target'.

# error if filter variable doesn't exist is pluralized properly

    Problem while computing `..1 = var == 5`..
    i Filter using the column 'gene'.

---

    Problem while computing `..1 = var == 5`..
    i Filter using the columns 'gene' and 'target'.

# chrom and gene are deprecated

    Code
      read_tbl_ref_alt_cov(.tbl_ref = "small.csv", .tbl_alt = "small.csv", .tbl_cov = "small.csv",
        chrom = "13")
    Condition
      Warning:
      The `chrom` argument of `read()` is deprecated as of miplicorn 0.1.0.
      Please use the `...` argument instead to filter data.
    Message
      Joining, by = "gene_id"
      Joining, by = "gene_id"
      Joining, by = "gene_id"
    Output
      # A ref alt cov table: 0 x 10
      # ... with 10 variables: sample <chr>, gene_id <chr>, gene <chr>,
      #   mutation <chr>, func <chr>, aa_chng <chr>, target <chr>,
      #   ref_umi_count <dbl>, alt_umi_count <dbl>, coverage <dbl>

---

    Code
      read_tbl_ref_alt_cov(.tbl_ref = "small.csv", .tbl_alt = "small.csv", .tbl_cov = "small.csv",
        gene = "atp")
    Condition
      Warning:
      The `gene` argument of `read()` is deprecated as of miplicorn 0.1.0.
      Please use the `...` argument instead to filter data.
    Message
      Joining, by = "gene"
      Joining, by = "gene"
      Joining, by = "gene"
    Output
      # A ref alt cov table: 2 x 9
        sample gene  mutation   func     aa_chng target ref_umi_count alt_umi_count
        <chr>  <chr> <chr>      <chr>    <chr>   <chr>          <dbl>         <dbl>
      1 S1     atp6  atp6-A623E missense A623E   Yes                0             0
      2 S2     atp6  atp6-A623E missense A623E   Yes                0             0
      # ... with 1 more variable: coverage <dbl>

---

    Code
      read_tbl_ref_alt_cov(.tbl_ref = "small.csv", .tbl_alt = "small.csv", .tbl_cov = "small.csv",
        chrom = "13", gene = "atp")
    Condition
      Warning:
      The `chrom` argument of `read()` is deprecated as of miplicorn 0.1.0.
      Please use the `...` argument instead to filter data.
      Warning:
      The `gene` argument of `read()` is deprecated as of miplicorn 0.1.0.
      Please use the `...` argument instead to filter data.
      Error in `read_tbl_ref_alt_cov()`:
      ! Multiple filtering criteria selected.
      x Cannot filter on both `chrom` and `gene`.
      i Select only one piece of information to filter on.

# read_tbl_ref_alt_cov returns error if a file is empty

    Unable to read files.
    x "empty-file" is an empty file.

---

    Unable to read files.
    x "empty-file" is an empty file.

