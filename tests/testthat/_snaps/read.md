# chrom and gene are deprecated

    Code
      suppressMessages(read("small.csv", "small.csv", "small.csv", chrom = "13",
        gene = "10"))
    Warning <lifecycle_warning_deprecated>
      The `chrom` argument of `read()` is deprecated as of miplicorn 0.0.0.9001.
      Please use the `...` argument instead to filter data.
      The `gene` argument of `read()` is deprecated as of miplicorn 0.0.0.9001.
      Please use the `...` argument instead to filter data.
    Error <rlang_error>
      Multiple filtering criteria selected.
      x Cannot filter on both `chrom` and `gene`.
      i Select only one piece of information to filter on.

# named filter inputs returns error

    Code
      read_file("small.csv", gene = "mdr1", .name = value)
    Error <rlang_error>
      Problem with `read()` input `..1`.
      x Input `..1` is named
      i This usually means that you've used `=` instead of `==`.
      i Did you mean `gene == "mdr1"`?

