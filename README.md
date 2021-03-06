# AcidSingleCell

Toolkit for single-cell RNA-seq analysis that extends the functionality of
[SingleCellExperiment][].

## Installation

Requirements: [R][] >= 4.0, [Bioconductor][] >= 3.12.

### [R][] method

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
install.packages(
    pkgs = "AcidSingleCell",
    repos = c(
        "https://r.acidgenomics.com",
        BiocManager::repositories()
    )
)
```

### [Docker][] method

```sh
image="acidgenomics/r-acidsinglecell"
workdir="/mnt/work"
docker pull "$image"
docker run -it \
    --volume="${PWD}:${workdir}" \
    --workdir="$workdir" \
    "$image" \
    R
```

[bioconda]: https://bioconda.github.io/
[bioconductor]: https://bioconductor.org/
[conda]: https://conda.io/
[docker]: https://www.docker.com/
[r]: https://www.r-project.org/
[singlecellexperiment]: http://bioconductor.org/packages/SingleCellExperiment/
