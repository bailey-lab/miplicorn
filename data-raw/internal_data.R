# Table for converting amino acid abbreviations
aa_conversion <- tibble::tibble(
  single = c(
    "G", "A", "L", "M", "F", "W", "K", "Q", "E", "S",
    "P", "V", "I", "C", "Y", "H", "R", "N", "D", "T"
  ),
  three = c(
    "Gly", "Ala", "Leu", "Met", "Phe", "Trp", "Lys",
    "Gln", "Glu", "Ser", "Pro", "Val", "Ile", "Cys",
    "Tyr", "His", "Arg", "Asn", "Asp", "Thr"
  )
)

usethis::use_data(aa_conversion, internal = TRUE, overwrite = TRUE)
