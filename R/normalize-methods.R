#' Normalize expression using pre-computed size factors
#'
#' This function calculates log-normalized size-factor-adjusted counts that
#' are defined in `logcounts()` from the raw, non-normalized count matrix
#' defined in `counts()`.
#'
#' If no library size factors are defined in `sizeFactors()`, these values will
#' be computed internally automatically using
#' `scuttle::computeLibraryFactors()`.
#'
#' @name normalize
#' @note Updated 2021-10-15.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return Modified object.
#'
#' @seealso
#' - `AcidExperiment::estimateSizeFactors()`.
#' - `scuttle::computeLibraryFactors()`.
#' - `scuttle::logNormCounts()`.
#' - `scuttle::librarySizeFactors()`.
#' - `scuttle::geometricSizeFactors()`.
#' - `scuttle::medianSizeFactors()`.
#' - `Seurat::NormalizeData()`.
#' - `monocle3::preprocess_cds()`.
#' - `monocle3::normalized_counts()`.
#' - `SingleCellExperiment::normcounts()`.
#' - `SingleCellExperiment::logcounts()`.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' object <- normalize(object)
#' head(sizeFactors(object))
#' logcounts(object)[seq_len(2L), seq_len(2L)]
NULL



#' Normalize a SingleCellExperiment
#'
#' @note Updated 2021-10-15.
#' @noRd
#'
#' @section Size factor calculation options (from scuttle documentation):
#'
#' The `librarySizeFactors()` function provides a simple definition of the size
#' factor for each cell, computed as the library size of each cell after scaling
#' them to have a mean of 1 across all cells. This is fast but inaccurate in the
#' presence of differential expression between cells that introduce composition
#' biases.
#'
#' The `geometricSizeFactors()` function instead computes the geometric
#' mean within each cell. This is more robust to composition biases but is only
#' accurate when the counts are large and there are few zeroes.
#'
#' The `medianSizeFactors()` function uses a DESeq2-esque approach based on the
#' median ratio from an average pseudo-cell. Briefly, we assume that most genes
#' are non-DE, such that any systematic fold difference in coverage (as defined
#' by the median ratio) represents technical biases that must be removed. This
#' is highly robust to composition biases but relies on sufficient sequencing
#' coverage to obtain well-defined ratios.
`normalize,SCE` <-  # nolint
    function(object) {
        validObject(object)
        requireNamespaces("scuttle")
        if (is.null(sizeFactors(object))) {
            alert(sprintf(
                fmt = paste(
                    "Generating {.val %s} using",
                    "{.pkg %s}::{.fun %s}."
                ),
                "sizeFactors",
                "scuttle", "computeLibraryFactors"
            ))
            object <- scuttle::computeLibraryFactors(object)
        }
        if (!isSubset("logcounts", assayNames(object))) {
            alert(sprintf(
                fmt = paste(
                    "Generating {.val %s} assay using",
                    "{.pkg %s}::{.fun %s}."
                ),
                "logcounts",
                "scuttle", "logNormCounts"
            ))
            object <- scuttle::logNormCounts(object)
        }
        assert(
            !is.null(sizeFactors(object)),
            isSubset("logcounts", assayNames(object))
        )
        object
    }



#' @rdname normalize
#' @export
setMethod(
    f = "normalize",
    signature = signature(object = "SingleCellExperiment"),
    definition = `normalize,SCE`
)
