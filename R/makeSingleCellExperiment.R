#' Make a SingleCellExperiment object
#'
#' @export
#' @note Updated 2021-02-05.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Passthrough arguments to
#'   [`makeSummarizedExperiment()`][AcidExperiment::makeSummarizedExperiment].
#'
#' @seealso
#' - `AcidExperiment::makeSummarizedExperiment()`.
#' - `SingleCellExperiment::altExps()`, which has replaced the now defunct
#'   `isSpike` method for setting spike-in transcripts.
#'
#' @return `SingleCellExperiment`.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#'
#' ## SimpleList ====
#' object <- SingleCellExperiment
#' assays <- SummarizedExperiment::assays(object)
#' rowRanges <- SummarizedExperiment::rowRanges(object)
#' colData <- SummarizedExperiment::colData(object)
#' metadata <- S4Vectors::metadata(object)
#' reducedDims <- SingleCellExperiment::reducedDims(object)
#'
#' x <- makeSingleCellExperiment(
#'     assays = assays,
#'     rowRanges = rowRanges,
#'     colData = colData,
#'     metadata = metadata,
#'     reducedDims = reducedDims
#' )
#' print(x)
makeSingleCellExperiment <- function(
    ...,
    reducedDims = S4Vectors::SimpleList()
) {
    assert(isAny(reducedDims, c("SimpleList", "list", "NULL")))
    if (!is(reducedDims, "SimpleList")) {
        reducedDims <- SimpleList(reducedDims)
    }
    ## Don't enforce camel case here, since it's currently common to slot PCA,
    ## TSNE, UMAP assays (note upper case).
    if (hasLength(reducedDims)) {
        assert(hasValidNames(reducedDims))
    }
    se <- makeSummarizedExperiment(...)
    assert(is(se, "SummarizedExperiment"))
    validObject(se)
    rowRanges <- rowRanges(se)
    args <- list(
        assays = assays(se),
        rowRanges = rowRanges,
        colData = colData(se),
        metadata = metadata(se),
        reducedDims = reducedDims
    )
    args <- Filter(f = Negate(is.null), x = args)
    sce <- do.call(what = SingleCellExperiment, args = args)
    assert(is(sce, "SingleCellExperiment"))
    validObject(sce)
    sce
}
