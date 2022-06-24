# AcidSingleCell

[![Install with Bioconda](https://img.shields.io/badge/install%20with-bioconda-brightgreen.svg?style=flat)](http://bioconda.github.io/recipes/r-acidsinglecell/README.html)

Toolkit for single-cell RNA-seq analysis that extends the functionality of
[SingleCellExperiment][].

## Installation

This is an [R][] package.

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
install.packages(
    pkgs = "AcidSingleCell",
    repos = c(
        "https://r.acidgenomics.com",
        BiocManager::repositories()
    ),
    dependencies = TRUE
)
```

### [Conda][] method

Configure [Conda][] to use the [Bioconda][] channels.

```sh
# Don't install recipe into base environment.
name='r-acidsinglecell'
conda create --name="$name" "$name"
conda activate "$name"
R
```

### [Docker][] method

```sh
image='acidgenomics/r-packages:acidsinglecell'
workdir='/mnt/work'
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
