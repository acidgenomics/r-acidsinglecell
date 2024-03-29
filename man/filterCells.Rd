% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/filterCells-methods.R
\name{filterCells}
\alias{filterCells}
\alias{filterCells,SingleCellExperiment-method}
\title{Filter cells}
\usage{
filterCells(object, ...)

\S4method{filterCells}{SingleCellExperiment}(
  object,
  assay = 1L,
  minCounts = 1L,
  maxCounts = Inf,
  minFeatures = 1L,
  maxFeatures = Inf,
  minNovelty = 0L,
  maxMitoRatio = 1L,
  minCellsPerFeature = 1L,
  nCells = Inf,
  countsCol = "nCount",
  featuresCol = "nFeature",
  noveltyCol = "log10FeaturesPerCount",
  mitoRatioCol = "mitoRatio"
)
}
\arguments{
\item{object}{Object.}

\item{assay}{\code{vector(1)}.
Assay name or index position.}

\item{minCounts, maxCounts}{\code{integer(1)}.
Minimum/maximum number of counts per cell.
Applies to UMI disambiguated counts for droplet scRNA-seq.
Matches \code{nUMI} then \code{nCount} column in \code{colData()} internally.
Previously named \code{minUMIs}/\code{maxUMIs} in bcbioSingleCell.}

\item{minFeatures, maxFeatures}{\code{integer(1)}.
Minimum/maximum number of features (i.e. genes) detected.
Matches \code{nFeature}in \code{colData()} internally.
Previously named \code{minGenes}/\code{maxGenes} in bcbioSingleCell.}

\item{minNovelty}{\code{integer(1)} (\code{0}-\code{1}).
Minimum novelty score (log10 features per UMI).
Matches \code{log10FeaturesPerCount} then \code{log10FeaturesPerUMI} (legacy)
\code{colData()} internally.}

\item{maxMitoRatio}{\code{integer(1)} (\code{0}-\code{1}).
Maximum relative mitochondrial abundance.}

\item{minCellsPerFeature}{\code{integer(1)}.
Include genes with non-zero expression in at least this many cells.
Previously named \code{minCellsPerGene} in bcbioSingleCell.}

\item{nCells}{\code{integer(1)}.
Expected number of cells per sample.
Don't set this by default, unless you're confident of your capture.}

\item{countsCol, featuresCol, noveltyCol, mitoRatioCol}{\code{character(1)}.
Column mapping name.}

\item{...}{Additional arguments.}
}
\value{
\code{SingleCellExperiment}.
}
\description{
Filter cells
}
\details{
Apply feature (i.e. gene/transcript) detection, novelty score, and
mitochondrial abundance cutoffs to cellular barcodes. By default we recommend
applying the same filtering cutoff to all samples. The filtering parameters
now support per-sample cutoffs, defined using a named \code{numeric} vector. When
matching per sample, be sure to use the \code{sampleNames()} return values (i.e.
the \code{sampleName} column in \code{sampleData()}.

Filtering information gets slotted into \code{metadata()} as \code{filterCells}
metadata.
}
\note{
Updated 2022-10-24.
}
\examples{
data(SingleCellExperiment_splatter, package = "AcidTest")

## SingleCellExperiment ====
object <- SingleCellExperiment_splatter
x <- filterCells(object)
print(x)

## Per sample cutoffs.
x <- filterCells(
    object = object,
    minCounts = c("sample1" = 100L)
)
print(x)
}
