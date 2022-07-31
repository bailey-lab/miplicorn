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

    The `chrom` argument of `read()` is deprecated as of miplicorn 0.1.0.
    Please use the `...` argument instead to filter data.

---

    The `gene` argument of `read()` is deprecated as of miplicorn 0.1.0.
    Please use the `...` argument instead to filter data.

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

