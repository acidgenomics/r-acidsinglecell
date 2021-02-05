#' Make a SingleCellExperiment object
#'
#' @inherit AcidExperiment::makeSummarizedExperiment
#'
#' @export
#' @note Updated 2021-02-05.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
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
#' assays <- assays(object)
#' rowRanges <- rowRanges(object)
#' colData <- colData(object)
#' metadata <- metadata(object)
#' reducedDims <- reducedDims(object)
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
    assays = S4Vectors::SimpleList(),
    rowRanges = GenomicRanges::GRanges(),
    colData = S4Vectors::DataFrame(),
    metadata = list(),
    reducedDims = S4Vectors::SSimpleList(),
    transgeneNames = NULL
) {
    assert(
        isAny(reducedDims, c("SimpleList", "list", "NULL"))
    )
    if (!is(reducedDims, "SimpleList")) {
        reducedDims <- SimpleList(reducedDims)
    }
    ## Don't enforce camel case here, since it's currently common to slot PCA,
    ## TSNE, UMAP assays (note upper case).
    if (hasLength(reducedDims)) {
        assert(hasValidNames(reducedDims))
    }
    se <- makeSummarizedExperiment(
        assays = assays,
        rowRanges = rowRanges,
        colData = colData,
        metadata = metadata,
        transgeneNames = transgeneNames
    )
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
