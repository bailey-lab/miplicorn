# error if lack ref, alt, coverage columns

    Data is mising required columns.
    x Need a column for the reference UMI counts.
    x Need a column for the alternate UMI counts.
    x Need a column for the coverage.

# error if lack mutation_name column

    Data needs the column `mutation_name`.

# data must have mutation_prev class

    Data object must be of class `mutation_prev`.
    x Its classes are `tbl_df`, `tbl`, and `data.frame`.
    i Did you forget to run `mutation_prevalence()` first?
