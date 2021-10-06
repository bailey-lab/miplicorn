#' @export
prep_haplotypes <- function(data,
                            sample = s_Sample,
                            probe = p_name,
                            haplotype = h_popUID,
                            rel_freq = c_AveragedFrac,
                            minPopSize = 3,
                            colorOuput = 11,
                            barHeight = 0.80) {
  # For each sample, mip, and haplotype, sum over all the data points
  # Gives us one relative freq value for each haplotype
  data_sum <- data %>%
    dplyr::mutate("{{sample}}" := factor({{ sample }})) %>%
    dplyr::group_by({{ sample }}, {{ probe }}, {{ haplotype }}) %>%
    dplyr::summarise("{{rel_freq}}" := sum({{ rel_freq }}))

  # Ensure relative column is a frequency. Divide abundance by total abundance
  data_rel <- data_sum %>%
    dplyr::group_by({{ sample }}, {{ probe }}) %>%
    dplyr::mutate(
      totalAbund = sum({{ rel_freq }}),
      "{{rel_freq}}" := {{ rel_freq }} / .data$totalAbund
    )

  # Find the number of haplotypes
  data_n_hap <- data_rel %>%
    dplyr::group_by({{ sample }}, {{ probe }}, {{ haplotype }}) %>%
    dplyr::mutate(s_COI = dplyr::n_distinct({{ haplotype }}))

  # Plotting info
  data_plot <- data_n_hap %>%
    dplyr::group_by({{ sample }}, {{ probe }}) %>%
    dplyr::mutate(
      relAbundCol_mod = {{ rel_freq }} * barHeight,
      fracCumSum = cumsum({{ rel_freq }}) - {{ rel_freq }},
      fracModCumSum = cumsum(.data$relAbundCol_mod) - .data$relAbundCol_mod,
      fakeFrac = 1 / unique(.data$s_COI),
      fakeFracMod = .data$fakeFrac * barHeight,
      fakeFracCumSum = cumsum(.data$fakeFrac) - .data$fakeFrac,
      fakeFracModCumSum = cumsum(.data$fakeFracMod) - .data$fakeFracMod
    ) %>%
    dplyr::ungroup()

  # Determine how many samples have a haplotype
  # Determine number of haplotypes per probe
  data_counts <- data_plot %>%
    dplyr::group_by({{ probe }}, {{ haplotype }}) %>%
    dplyr::summarise(samp_n = dplyr::n()) %>%
    dplyr::arrange({{ probe }}, dplyr::desc(.data$samp_n)) %>%
    dplyr::group_by({{ probe }}) %>%
    dplyr::mutate(
      popid = dplyr::row_number(),
      maxPopid = max(.data$popid)
    )

  data_join <- data_plot %>%
    dplyr::left_join(data_counts)

  # Filter based on minimum population size
  # (number of probes with the haplotype)
  data_filter <- data_join %>%
    dplyr::filter(.data$maxPopid >= minPopSize) %>%
    dplyr::mutate("{{probe}}" := factor({{ probe }}))

  # From here on, we deal with determining the colors of each haplotype
  # The shading code looks different here... L153 in original code
  colorsOutput <- colorOuput
  targetNumber <- 0
  targetToHue <- tibble::tibble(
    "{{probe}}" := character(),
    hueMod = double()
  )
  tempTarCol <- dplyr::pull(data_filter, {{ probe }})

  for (tarname in levels(tempTarCol)) {
    targetToHue <- targetToHue %>%
      tibble::add_row(
        "{{probe}}" := tarname,
        hueMod = (targetNumber %% colorsOutput) + 1
      )
    targetNumber <- targetNumber + 1
  }

  data_filter <- data_filter %>%
    dplyr::group_by({{ probe }}) %>%
    dplyr::mutate(popidFrac = (.data$popid - 1) / (.data$maxPopid))

  tempTarCol <- dplyr::pull(data_filter, {{ probe }})

  targetToHue <- targetToHue %>%
    dplyr::mutate("{{probe}}" := factor({{ probe }}, levels = levels(tempTarCol)))

  data_final <- data_filter %>%
    dplyr::left_join(targetToHue) %>%
    dplyr::mutate(
      popidPerc = 100 * .data$popidFrac,
      popidFracRegColor = round(abs((.data$popidPerc + (.data$hueMod / colorsOutput) * 100) %% 200 - 0.0001) %% 100),
      popidPercLog = log((.data$popidFrac * 99) + 1, base = 100) * 100,
      popidFracLogColor = round(abs((.data$popidPercLog + (.data$hueMod / colorsOutput) * 100) %% 200 - 0.0001) %% 100)
    )

  # Only keep needed columns to not confuse the user
  dplyr::select(
    data_final,
    {{ sample }},
    {{ probe }},
    {{ haplotype }},
    .data$fracModCumSum,
    .data$relAbundCol_mod,
    .data$popidFracLogColor
  )
}

#' @export
plot_haplotypes <- function(data,
                            sample = s_Sample,
                            probe = p_name,
                            haplotype = h_popUID,
                            rel_freq = c_AveragedFrac,
                            colors = RColorBrewer::brewer.pal(11, "Spectral")) {
  # Prepare the data
  # data <- prep_haplotypes(data)

  # Find unique samples to label the y-axis
  unique_samples <- data %>%
    dplyr::ungroup() %>%
    dplyr::select({{ sample }}) %>%
    unique() %>%
    dplyr::arrange({{ sample }}) %>%
    dplyr::pull()

  # Plot
  ggplot2::ggplot(data) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = as.numeric({{ probe }}) - 0.5,
        xmax = as.numeric({{ probe }}) + 0.5,
        ymin = as.numeric({{ sample }}) + .data$fracModCumSum - 0.5,
        ymax = as.numeric({{ sample }}) + .data$fracModCumSum + .data$relAbundCol_mod - 0.5,
        fill = .data$popidFracLogColor
      ),
      color = "black"
    ) +
    ggplot2::scale_fill_gradientn(colours = colors) +
    # Used for shading
    # ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous(
      breaks = seq_along(unique_samples),
      labels = unique_samples
    ) +
    rainbow_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      legend.position = "none"
    )
}

# plot_haplotypes <- function(data,
#                             colors = RColorBrewer::brewer.pal(11, "Spectral"),
#                             ...) {
#   prep_data <- prep_haplotypes(data, ...)
