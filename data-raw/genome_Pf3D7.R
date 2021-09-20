# File path
file <- system.file(
  "extdata",
  "PlasmoDB-53_Pfalciparum3D7.gff",
  package = "miplicorn",
  mustWork = TRUE
)

# Create tibble containing genome information
genome_Pf3D7 <- readr::read_tsv(
  file = file,
  col_names = FALSE,
  skip = 2,
  n_max = 16
) %>%
  tidyr::separate(
    col = X1,
    into = c("feature", "chrom", "num", "size"),
    sep = "\\s"
  ) %>%
  dplyr::select(-feature) %>%
  dplyr::rename(start = num, end = size) %>%
  dplyr::mutate(chrom = c(paste0("chr", seq(1:14)), "chrapi", "chrmit"),
                dplyr::across(start:end, as.integer))

# Store data in package
usethis::use_data(genome_Pf3D7, overwrite = TRUE)
