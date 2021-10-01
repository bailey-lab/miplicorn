# error if packages not intalled

    Code
      plot_chromoMap(genome_Pf3D7, probes)
    Error <rlang_error>
      Packages "chromoMap" and "withr" needed to create chromosome maps.
      Please install them.

---

    Code
      plot_karyoploteR(genome_Pf3D7, probes)
    Error <rlang_error>
      Package "karyoploteR" needed to create chromosome maps. Please install
      it.

# error if genome is misformatted

    Code
      chromosome_map(genome_Pf3D7[, -1], probes, "chromoMap")
    Error <rlang_error>
      Genomic information is misformatted.

---

    Code
      chromosome_map(genome_Pf3D7[, -1], probes, "karyoploteR")
    Error <rlang_error>
      Genomic information is misformatted.

# error if probes is misformatted

    Code
      chromosome_map(genome_Pf3D7, probes[, -4], "chromoMap")
    Error <rlang_error>
      Annotation information is misformatted.
      i Did you forget to indicate the probe sets?

---

    Code
      chromosome_map(genome_Pf3D7, probes[, -4], "karyoploteR")
    Error <rlang_error>
      Annotation information is misformatted.
      i Did you forget to indicate the probe sets?

# error if wrong mapping package specified

    Code
      chromosome_map(genome_Pf3D7, probes)
    Error <rlang_error>
      `map_pkg` must be of length 1.
      i `map_pkg` must be either "chromoMap" or "karyoploteR".

---

    Code
      chromosome_map(genome_Pf3D7, probes, "ggplot2")
    Error <rlang_error>
      `map_pkg` must be either "chromoMap" or "karyoploteR".
      x You've input "ggplot2".

