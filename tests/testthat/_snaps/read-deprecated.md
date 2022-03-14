# chrom and gene are deprecated

    Code
      read("small.csv", "small.csv", "small.csv", chrom = "13")
    Warning <lifecycle_warning_deprecated>
      `read()` was deprecated in miplicorn 0.2.0.
      Please use `read_tbl_ref_alt_cov()` instead.
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
      read("small.csv", "small.csv", "small.csv", chrom = "atp")
    Warning <lifecycle_warning_deprecated>
      `read()` was deprecated in miplicorn 0.2.0.
      Please use `read_tbl_ref_alt_cov()` instead.
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
      read("small.csv", "small.csv", "small.csv", chrom = "13", gene = "atp")
    Warning <lifecycle_warning_deprecated>
      `read()` was deprecated in miplicorn 0.2.0.
      Please use `read_tbl_ref_alt_cov()` instead.
      The `chrom` argument of `read()` is deprecated as of miplicorn 0.1.0.
      Please use the `...` argument instead to filter data.
      The `gene` argument of `read()` is deprecated as of miplicorn 0.1.0.
      Please use the `...` argument instead to filter data.
    Error <rlang_error>
      Multiple filtering criteria selected.
      x Cannot filter on both `chrom` and `gene`.
      i Select only one piece of information to filter on.

# read_file() is deprecated

    Code
      read_file("small.csv", .name = "ref_umi_count")
    Warning <lifecycle_warning_deprecated>
      `read_file()` was deprecated in miplicorn 0.2.0.
      The function has been replaced by three more specific functions:
       `read_tbl_reference()`, `read_tbl_alternate()`, and `read_tbl_coverage()`.
    Message <rlang_message>
      Input detected as the reference table.
    Output
      # A tibble: 4 x 8
        sample gene_id gene  mutation   func     aa_chng target ref_umi_count
        <chr>  <chr>   <chr> <chr>      <chr>    <chr>   <chr>          <dbl>
      1 S1     Site1   atp6  atp6-A623E missense A623E   Yes                0
      2 S2     Site1   atp6  atp6-A623E missense A623E   Yes                0
      3 S1     Site2   mdr1  mdr1-N86Y  sense    N86Y    Yes               13
      4 S2     Site2   mdr1  mdr1-N86Y  sense    N86Y    Yes                0

# deprecated read_file() detects reference tables

    Code
      read_file("small.csv", .name = "ref_umi_count")
    Warning <lifecycle_warning_deprecated>
      `read_file()` was deprecated in miplicorn 0.2.0.
      The function has been replaced by three more specific functions:
       `read_tbl_reference()`, `read_tbl_alternate()`, and `read_tbl_coverage()`.
    Message <rlang_message>
      Input detected as the reference table.
    Output
      # A tibble: 4 x 8
        sample gene_id gene  mutation   func     aa_chng target ref_umi_count
        <chr>  <chr>   <chr> <chr>      <chr>    <chr>   <chr>          <dbl>
      1 S1     Site1   atp6  atp6-A623E missense A623E   Yes                0
      2 S2     Site1   atp6  atp6-A623E missense A623E   Yes                0
      3 S1     Site2   mdr1  mdr1-N86Y  sense    N86Y    Yes               13
      4 S2     Site2   mdr1  mdr1-N86Y  sense    N86Y    Yes                0

# deprecated read_file() detects alternate tables

    Code
      read_file("small.csv", .name = "alt_umi_count")
    Warning <lifecycle_warning_deprecated>
      `read_file()` was deprecated in miplicorn 0.2.0.
      The function has been replaced by three more specific functions:
       `read_tbl_reference()`, `read_tbl_alternate()`, and `read_tbl_coverage()`.
    Message <rlang_message>
      Input detected as the alternate table.
    Output
      # A tibble: 4 x 8
        sample gene_id gene  mutation   func     aa_chng target alt_umi_count
        <chr>  <chr>   <chr> <chr>      <chr>    <chr>   <chr>          <dbl>
      1 S1     Site1   atp6  atp6-A623E missense A623E   Yes                0
      2 S2     Site1   atp6  atp6-A623E missense A623E   Yes                0
      3 S1     Site2   mdr1  mdr1-N86Y  sense    N86Y    Yes               13
      4 S2     Site2   mdr1  mdr1-N86Y  sense    N86Y    Yes                0

# deprecated read_file() detects coverage tables

    Code
      read_file("small.csv", .name = "coverage")
    Warning <lifecycle_warning_deprecated>
      `read_file()` was deprecated in miplicorn 0.2.0.
      The function has been replaced by three more specific functions:
       `read_tbl_reference()`, `read_tbl_alternate()`, and `read_tbl_coverage()`.
    Message <rlang_message>
      Input detected as the coverage table.
    Output
      # A tibble: 4 x 8
        sample gene_id gene  mutation   func     aa_chng target coverage
        <chr>  <chr>   <chr> <chr>      <chr>    <chr>   <chr>     <dbl>
      1 S1     Site1   atp6  atp6-A623E missense A623E   Yes           0
      2 S2     Site1   atp6  atp6-A623E missense A623E   Yes           0
      3 S1     Site2   mdr1  mdr1-N86Y  sense    N86Y    Yes          13
      4 S2     Site2   mdr1  mdr1-N86Y  sense    N86Y    Yes           0

# deprecated read_file() fails if can't detect input

    Code
      read_file("small.csv")
    Warning <lifecycle_warning_deprecated>
      `read_file()` was deprecated in miplicorn 0.2.0.
      The function has been replaced by three more specific functions:
       `read_tbl_reference()`, `read_tbl_alternate()`, and `read_tbl_coverage()`.
    Error <rlang_error>
      Unable to detect the type of table input.
