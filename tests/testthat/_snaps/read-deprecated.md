# read() is deprecated

    `read()` was deprecated in miplicorn 0.2.0.
    Please use `read_tbl_ref_alt_cov()` instead.

# read_file() is deprecated

    Code
      read_file("small.csv", .name = "ref_umi_count")
    Condition
      Warning:
      `read_file()` was deprecated in miplicorn 0.2.0.
      The function has been replaced by three more specific functions:
       `read_tbl_reference()`, `read_tbl_alternate()`, and `read_tbl_coverage()`.
    Message
      Input detected as the reference table.
    Output
      # A reference table: 4 x 8
        sample gene_id gene  mutation   func     aa_chng target ref_umi_count
        <chr>  <chr>   <chr> <chr>      <chr>    <chr>   <chr>          <dbl>
      1 S1     Site1   atp6  atp6-A623E missense A623E   Yes                0
      2 S2     Site1   atp6  atp6-A623E missense A623E   Yes                0
      3 S1     Site2   mdr1  mdr1-N86Y  sense    N86Y    Yes               13
      4 S2     Site2   mdr1  mdr1-N86Y  sense    N86Y    Yes                0

# deprecated read_file() detects reference tables

    Code
      read_file("small.csv", .name = "ref_umi_count")
    Condition
      Warning:
      `read_file()` was deprecated in miplicorn 0.2.0.
      The function has been replaced by three more specific functions:
       `read_tbl_reference()`, `read_tbl_alternate()`, and `read_tbl_coverage()`.
    Message
      Input detected as the reference table.
    Output
      # A reference table: 4 x 8
        sample gene_id gene  mutation   func     aa_chng target ref_umi_count
        <chr>  <chr>   <chr> <chr>      <chr>    <chr>   <chr>          <dbl>
      1 S1     Site1   atp6  atp6-A623E missense A623E   Yes                0
      2 S2     Site1   atp6  atp6-A623E missense A623E   Yes                0
      3 S1     Site2   mdr1  mdr1-N86Y  sense    N86Y    Yes               13
      4 S2     Site2   mdr1  mdr1-N86Y  sense    N86Y    Yes                0

# deprecated read_file() detects alternate tables

    Code
      read_file("small.csv", .name = "alt_umi_count")
    Condition
      Warning:
      `read_file()` was deprecated in miplicorn 0.2.0.
      The function has been replaced by three more specific functions:
       `read_tbl_reference()`, `read_tbl_alternate()`, and `read_tbl_coverage()`.
    Message
      Input detected as the alternate table.
    Output
      # An alternate table: 4 x 8
        sample gene_id gene  mutation   func     aa_chng target alt_umi_count
        <chr>  <chr>   <chr> <chr>      <chr>    <chr>   <chr>          <dbl>
      1 S1     Site1   atp6  atp6-A623E missense A623E   Yes                0
      2 S2     Site1   atp6  atp6-A623E missense A623E   Yes                0
      3 S1     Site2   mdr1  mdr1-N86Y  sense    N86Y    Yes               13
      4 S2     Site2   mdr1  mdr1-N86Y  sense    N86Y    Yes                0

# deprecated read_file() detects coverage tables

    Code
      read_file("small.csv", .name = "coverage")
    Condition
      Warning:
      `read_file()` was deprecated in miplicorn 0.2.0.
      The function has been replaced by three more specific functions:
       `read_tbl_reference()`, `read_tbl_alternate()`, and `read_tbl_coverage()`.
    Message
      Input detected as the coverage table.
    Output
      # A coverage table: 4 x 8
        sample gene_id gene  mutation   func     aa_chng target coverage
        <chr>  <chr>   <chr> <chr>      <chr>    <chr>   <chr>     <dbl>
      1 S1     Site1   atp6  atp6-A623E missense A623E   Yes           0
      2 S2     Site1   atp6  atp6-A623E missense A623E   Yes           0
      3 S1     Site2   mdr1  mdr1-N86Y  sense    N86Y    Yes          13
      4 S2     Site2   mdr1  mdr1-N86Y  sense    N86Y    Yes           0

# deprecated read_file() fails if can't detect input

    Code
      read_file("small.csv")
    Condition
      Warning:
      `read_file()` was deprecated in miplicorn 0.2.0.
      The function has been replaced by three more specific functions:
       `read_tbl_reference()`, `read_tbl_alternate()`, and `read_tbl_coverage()`.
      Error in `read_file()`:
      ! Unable to detect the type of table input.

