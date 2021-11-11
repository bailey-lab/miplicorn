# chrom and gene are deprecated

    Code
      suppressMessages(read("small.csv", "small.csv", "small.csv", chrom = "13",
        gene = "10"))
    Warning <lifecycle_warning_deprecated>
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
    Message <message>
      Input detected as the reference table.
    Output
      # A tibble: 4 x 8
        sample     gene_id       gene  mutation_name  exonic_func   aa_change targeted
        <chr>      <chr>         <chr> <chr>          <chr>         <chr>     <chr>   
      1 d10_jjj_23 PF3D7_0106300 atp6  atp6-Ala623Glu missense_var~ Ala623Glu Yes     
      2 d10_jjj_43 PF3D7_0106300 atp6  atp6-Ala623Glu missense_var~ Ala623Glu Yes     
      3 d10_jjj_23 PF3D7_0523000 mdr1  mdr1-Asn86Tyr  missense_var~ Asn86Tyr  Yes     
      4 d10_jjj_43 PF3D7_0523000 mdr1  mdr1-Asn86Tyr  missense_var~ Asn86Tyr  Yes     
      # ... with 1 more variable: ref_umi_count <dbl>

# deprecated read_file() detects correct inputs

    Code
      read_file("small.csv", .name = "ref_umi_count")
    Warning <lifecycle_warning_deprecated>
      `read_file()` was deprecated in miplicorn 0.2.0.
      The function has been replaced by three more specific functions:
       `read_tbl_reference()`, `read_tbl_alternate()`, and `read_tbl_coverage()`.
    Message <message>
      Input detected as the reference table.
    Output
      # A tibble: 4 x 8
        sample     gene_id       gene  mutation_name  exonic_func   aa_change targeted
        <chr>      <chr>         <chr> <chr>          <chr>         <chr>     <chr>   
      1 d10_jjj_23 PF3D7_0106300 atp6  atp6-Ala623Glu missense_var~ Ala623Glu Yes     
      2 d10_jjj_43 PF3D7_0106300 atp6  atp6-Ala623Glu missense_var~ Ala623Glu Yes     
      3 d10_jjj_23 PF3D7_0523000 mdr1  mdr1-Asn86Tyr  missense_var~ Asn86Tyr  Yes     
      4 d10_jjj_43 PF3D7_0523000 mdr1  mdr1-Asn86Tyr  missense_var~ Asn86Tyr  Yes     
      # ... with 1 more variable: ref_umi_count <dbl>

---

    Code
      read_file("small.csv", .name = "alt_umi_count")
    Warning <lifecycle_warning_deprecated>
      `read_file()` was deprecated in miplicorn 0.2.0.
      The function has been replaced by three more specific functions:
       `read_tbl_reference()`, `read_tbl_alternate()`, and `read_tbl_coverage()`.
    Message <message>
      Input detected as the alternate table.
    Output
      # A tibble: 4 x 8
        sample     gene_id       gene  mutation_name  exonic_func   aa_change targeted
        <chr>      <chr>         <chr> <chr>          <chr>         <chr>     <chr>   
      1 d10_jjj_23 PF3D7_0106300 atp6  atp6-Ala623Glu missense_var~ Ala623Glu Yes     
      2 d10_jjj_43 PF3D7_0106300 atp6  atp6-Ala623Glu missense_var~ Ala623Glu Yes     
      3 d10_jjj_23 PF3D7_0523000 mdr1  mdr1-Asn86Tyr  missense_var~ Asn86Tyr  Yes     
      4 d10_jjj_43 PF3D7_0523000 mdr1  mdr1-Asn86Tyr  missense_var~ Asn86Tyr  Yes     
      # ... with 1 more variable: alt_umi_count <dbl>

---

    Code
      read_file("small.csv", .name = "coverage")
    Warning <lifecycle_warning_deprecated>
      `read_file()` was deprecated in miplicorn 0.2.0.
      The function has been replaced by three more specific functions:
       `read_tbl_reference()`, `read_tbl_alternate()`, and `read_tbl_coverage()`.
    Message <message>
      Input detected as the coverage table.
    Output
      # A tibble: 4 x 8
        sample     gene_id gene  mutation_name exonic_func aa_change targeted coverage
        <chr>      <chr>   <chr> <chr>         <chr>       <chr>     <chr>       <dbl>
      1 d10_jjj_23 PF3D7_~ atp6  atp6-Ala623G~ missense_v~ Ala623Glu Yes             0
      2 d10_jjj_43 PF3D7_~ atp6  atp6-Ala623G~ missense_v~ Ala623Glu Yes             0
      3 d10_jjj_23 PF3D7_~ mdr1  mdr1-Asn86Tyr missense_v~ Asn86Tyr  Yes            13
      4 d10_jjj_43 PF3D7_~ mdr1  mdr1-Asn86Tyr missense_v~ Asn86Tyr  Yes             0

# deprecated read_file() fails if can't detect input

    Unable to detect the type of table input.

