# tbl_sum methods work

    Code
      print(ref)
    Output
      # A reference table: 2 x 2
        sample  ref_umi_count
        <chr>           <dbl>
      1 sample1            11
      2 sample2           100

---

    Code
      print(alt)
    Output
      # An alternate table: 2 x 2
        sample  alt_umi_count
        <chr>           <dbl>
      1 sample1            11
      2 sample2           100

---

    Code
      print(cov)
    Output
      # A coverage table: 2 x 2
        sample  coverage
        <chr>      <dbl>
      1 sample1       11
      2 sample2      100

---

    Code
      print(geno)
    Output
      # A genotype table: 2 x 3
        sample  targeted genotype
        <chr>   <chr>       <dbl>
      1 sample1 No              1
      2 sample2 Yes             0

---

    Code
      print(hap)
    Output
      # A haplotype table: 2 x 3
        sample  haplotype_id haplotype_sequence
        <chr>   <chr>        <chr>             
      1 sample1 mip1         CATG              
      2 sample2 mip2         TTGG              

---

    Code
      print(ref_alt_cov)
    Output
      # A ref alt cov table: 2 x 4
        sample  ref_umi_count alt_umi_count coverage
        <chr>           <dbl>         <dbl>    <dbl>
      1 sample1             5             1        6
      2 sample2             0             5        6

