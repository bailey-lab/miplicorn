# error if wrong mapping package specified

    Code
      chromosome_map(genome_Pf3D7, probes)
    Condition
      Error in `chromosome_map()`:
      ! `map_pkg` must be of length 1.
      i `map_pkg` must be either "chromoMap" or "karyoploteR".

---

    Code
      chromosome_map(genome_Pf3D7, probes, "ggplot2")
    Condition
      Error in `chromosome_map()`:
      ! `map_pkg` must be either "chromoMap" or "karyoploteR".
      x You've input "ggplot2".

# chromosome_map is deprecated

    Code
      chromosome_map(genome_Pf3D7[, -1], probes, "karyoploteR")
    Condition
      Warning:
      `chromosome_map()` was deprecated in miplicorn 0.2.0.
      The function has been deprecated in favor of `plot_chromoMap()` and
       `plot_karyoploteR()`.
      Error in `chromosome_map()`:
      ! Genomic information is misformatted.

---

    Code
      chromosome_map(genome_Pf3D7[, -1], probes, "chromoMap")
    Condition
      Warning:
      `chromosome_map()` was deprecated in miplicorn 0.2.0.
      The function has been deprecated in favor of `plot_chromoMap()` and
       `plot_karyoploteR()`.
      Error in `chromosome_map()`:
      ! Genomic information is misformatted.

