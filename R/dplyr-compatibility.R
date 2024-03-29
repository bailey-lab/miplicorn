# ref_tbl class ----------------------------------------------------------------
rlang::on_load(
  vctrs::s3_register(
    "dplyr::dplyr_reconstruct",
    "ref_tbl",
    method = ref_tbl_reconstruct
  )
)

ref_tbl_reconstruct <- function(data, template) {
  if (ref_tbl_reconstructable(data)) {
    new_ref_tbl(data)
  } else {
    tibble::new_tibble(data)
  }
}

ref_tbl_reconstructable <- function(data) {
  rlang::is_list(data) &&
    has_ref_tbl_cols(data) &&
    has_ref_tbl_coltypes(data)
}

has_ref_tbl_cols <- function(x) {
  cols <- c("sample", "ref_umi_count")
  all(cols %in% colnames(x))
}

has_ref_tbl_coltypes <- function(x) {
  coltypes <- c(
    sample = rlang::is_character(x$sample),
    ref_umi_count = rlang::is_double(x$ref_umi_count)
  )
  all(coltypes)
}

# alt_tbl class ----------------------------------------------------------------
rlang::on_load(
  vctrs::s3_register(
    "dplyr::dplyr_reconstruct",
    "alt_tbl",
    method = alt_tbl_reconstruct
  )
)

alt_tbl_reconstruct <- function(data, template) {
  if (alt_tbl_reconstructable(data)) {
    new_alt_tbl(data)
  } else {
    tibble::new_tibble(data)
  }
}

alt_tbl_reconstructable <- function(data) {
  rlang::is_list(data) &&
    has_alt_tbl_cols(data) &&
    has_alt_tbl_coltypes(data)
}

has_alt_tbl_cols <- function(x) {
  cols <- c("sample", "alt_umi_count")
  all(cols %in% colnames(x))
}

has_alt_tbl_coltypes <- function(x) {
  coltypes <- c(
    sample = rlang::is_character(x$sample),
    alt_umi_count = rlang::is_double(x$alt_umi_count)
  )
  all(coltypes)
}

# cov_tbl class ----------------------------------------------------------------
rlang::on_load(
  vctrs::s3_register(
    "dplyr::dplyr_reconstruct",
    "cov_tbl",
    method = cov_tbl_reconstruct
  )
)

cov_tbl_reconstruct <- function(data, template) {
  if (cov_tbl_reconstructable(data)) {
    new_cov_tbl(data)
  } else {
    tibble::new_tibble(data)
  }
}

cov_tbl_reconstructable <- function(data) {
  rlang::is_list(data) &&
    has_cov_tbl_cols(data) &&
    has_cov_tbl_coltypes(data)
}

has_cov_tbl_cols <- function(x) {
  cols <- c("sample", "coverage")
  all(cols %in% colnames(x))
}

has_cov_tbl_coltypes <- function(x) {
  coltypes <- c(
    sample = rlang::is_character(x$sample),
    coverage = rlang::is_double(x$coverage)
  )
  all(coltypes)
}

# geno-tbl class ---------------------------------------------------------------
rlang::on_load(
  vctrs::s3_register(
    "dplyr::dplyr_reconstruct",
    "geno_tbl",
    method = geno_tbl_reconstruct
  )
)

geno_tbl_reconstruct <- function(data, template) {
  if (geno_tbl_reconstructable(data)) {
    new_geno_tbl(data)
  } else {
    tibble::new_tibble(data)
  }
}

geno_tbl_reconstructable <- function(data) {
  rlang::is_list(data) &&
    has_geno_tbl_cols(data) &&
    has_geno_tbl_coltypes(data) &&
    has_genotype_vals(data$genotype)
}

has_geno_tbl_cols <- function(x) {
  cols <- c("sample", "genotype")
  all(cols %in% colnames(x))
}

has_geno_tbl_coltypes <- function(x) {
  coltypes <- c(
    sample = rlang::is_character(x$sample),
    genotype = rlang::is_double(x$genotype)
  )
  all(coltypes)
}

has_genotype_vals <- function(x) {
  all(unique(x) %in% c(NA, -1, 0, 1, 2))
}

# hap_tbl class ----------------------------------------------------------------
rlang::on_load(
  vctrs::s3_register(
    "dplyr::dplyr_reconstruct",
    "hap_tbl",
    method = hap_tbl_reconstruct
  )
)

hap_tbl_reconstruct <- function(data, template) {
  if (hap_tbl_reconstructable(data)) {
    new_hap_tbl(data)
  } else {
    tibble::new_tibble(data)
  }
}

hap_tbl_reconstructable <- function(data) {
  rlang::is_list(data) &&
    has_hap_tbl_cols(data) &&
    has_hap_tbl_coltypes(data)
}

has_hap_tbl_cols <- function(x) {
  # TODO: add other necessary columns
  cols <- c("sample", "haplotype_id", "haplotype_sequence")
  all(cols %in% colnames(x))
}

has_hap_tbl_coltypes <- function(x) {
  # TODO: add other necessary columns
  coltypes <- c(
    sample = rlang::is_character(x$sample),
    haplotype_id = rlang::is_character(x$haplotype_id),
    haplotype_sequence = rlang::is_character(x$haplotype_id)
  )
  all(coltypes)
}

# ref_alt_cov_tbl class --------------------------------------------------------
rlang::on_load(
  vctrs::s3_register(
    "dplyr::dplyr_reconstruct",
    "ref_alt_cov_tbl",
    method = ref_alt_cov_tbl_reconstruct
  )
)

ref_alt_cov_tbl_reconstruct <- function(data, template) {
  if (ref_alt_cov_tbl_reconstructable(data)) {
    new_ref_alt_cov_tbl(data)
  } else {
    tibble::new_tibble(data)
  }
}

ref_alt_cov_tbl_reconstructable <- function(data) {
  rlang::is_list(data) &&
    has_ref_alt_cov_tbl_cols(data) &&
    has_ref_alt_cov_tbl_coltypes(data)
}

has_ref_alt_cov_tbl_cols <- function(x) {
  cols <- c("sample", "ref_umi_count", "alt_umi_count", "coverage")
  all(cols %in% colnames(x))
}

has_ref_alt_cov_tbl_coltypes <- function(x) {
  coltypes <- c(
    sample = rlang::is_character(x$sample),
    ref_umi_count = rlang::is_double(x$ref_umi_count),
    alt_umi_count = rlang::is_double(x$alt_umi_count),
    coverage = rlang::is_double(x$coverage)
  )
  all(coltypes)
}

# mut-prev class ---------------------------------------------------------------
rlang::on_load(
  vctrs::s3_register(
    "dplyr::dplyr_reconstruct",
    "mut_prev",
    method = mut_prev_reconstruct
  )
)

mut_prev_reconstruct <- function(data, template) {
  if (mut_prev_reconstructable(data)) {
    new_mut_prev(data)
  } else {
    tibble::new_tibble(data)
  }
}

mut_prev_reconstructable <- function(data) {
  rlang::is_list(data) &&
    has_mut_prev_cols(data) &&
    has_mut_prev_coltypes(data)
}

has_mut_prev_cols <- function(x) {
  mut_prev_cols <- c("mutation_name", "prevalence")
  all(mut_prev_cols %in% colnames(x))
}

has_mut_prev_coltypes <- function(x) {
  coltypes <- c(
    mutation_name = rlang::is_character(x$mutation_name),
    prevalence = rlang::is_double(x$prevalence)
  )
  all(coltypes)
}

# mut-freq class ---------------------------------------------------------------
rlang::on_load(
  vctrs::s3_register(
    "dplyr::dplyr_reconstruct",
    "mut_freq",
    method = mut_freq_reconstruct
  )
)

mut_freq_reconstruct <- function(data, template) {
  if (mut_freq_reconstructable(data)) {
    new_mut_freq(data)
  } else {
    tibble::new_tibble(data)
  }
}

mut_freq_reconstructable <- function(data) {
  rlang::is_list(data) &&
    has_mut_freq_cols(data) &&
    has_mut_freq_coltypes(data)
}

has_mut_freq_cols <- function(x) {
  mut_freq_cols <- c("mutation_name", "frequency")
  all(mut_freq_cols %in% colnames(x))
}

has_mut_freq_coltypes <- function(x) {
  coltypes <- c(
    mutation_name = rlang::is_character(x$mutation_name),
    frequency = rlang::is_double(x$frequency)
  )
  all(coltypes)
}
