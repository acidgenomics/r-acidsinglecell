# AcidSingleCell

Toolkit for single-cell RNA-seq analysis that extends the functionality of
[SingleCellExperiment][].

## Installation

Requirements: [R][] >= 4.0, [Bioconductor][] >= 3.11.

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

[bioconductor]: https://bioconductor.org/
[r]: https://www.r-project.org/
[singlecellexperiment]: http://bioconductor.org/packages/SingleCellExperiment/
