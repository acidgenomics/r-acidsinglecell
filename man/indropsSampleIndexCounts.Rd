% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indropsSampleIndexCounts.R
\name{indropsSampleIndexCounts}
\alias{indropsSampleIndexCounts}
\title{Get the inDrops sample index counts from i5 FASTQ files}
\usage{
indropsSampleIndexCounts(dir, r3Pattern = "_R3\\\\.fastq\\\\.gz$", n = 24L)
}
\arguments{
\item{dir}{\code{character(1)}.
Directory containing inDrops FASTQ files.
Must contain R3 files, which contain the i5 index barcodes.}

\item{r3Pattern}{\code{character(1)}.
Tail pattern for matching the R3 FASTQ files.
Passed to \code{list.files()} internally as \code{pattern} argument.}

\item{n}{\code{integer(1)}.
Number of top barcodes to return.}
}
\value{
\code{SimpleList}.
Names correspond to matched file names.
}
\description{
Get the inDrops sample index counts from i5 FASTQ files
}
\note{
Updated 2023-10-27.
}
\examples{
## > indropsSampleIndexCounts(dir = file.path("indrops", "fastq"))
}
