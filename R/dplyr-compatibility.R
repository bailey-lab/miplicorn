# mut-prev class
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

# mut-freq class
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
