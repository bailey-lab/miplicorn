# error if packages not intalled

    Code
      chromosome_map(genome_Pf3D7, probes)
    Error <rlang_error>
      Packages "chromoMap" and "withr" needed to create chromosome maps. Please install them.

# error if genome is misformatted

    Code
      chromosome_map(genome_Pf3D7[, -1], probes)
    Error <rlang_error>
      Genomic information is misformatted.

# error if probes is misformatted

    Code
      chromosome_map(genome_Pf3D7, probes[, -4])
    Error <rlang_error>
      Annotation information is misformatted.
      i Did you forget to indicate the probe sets?

