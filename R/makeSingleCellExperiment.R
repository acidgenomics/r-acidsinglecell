#' Make a SingleCellExperiment object
#'
#' @export
#' @note Updated 2022-03-02.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Passthrough arguments to `makeSummarizedExperiment()`.
#'
#' @seealso
#' - `AcidExperiment::makeSummarizedExperiment()`.
#' - `SingleCellExperiment::altExps()`, which has replaced the now defunct
#' `isSpike` method for setting spike-in transcripts.
#'
#' @return `SingleCellExperiment`.
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SimpleList ====
#' object <- SingleCellExperiment_splatter
#' args <- list(
#'     "assays" = SummarizedExperiment::assays(object),
#'     "rowRanges" = SummarizedExperiment::rowRanges(object),
#'     "colData" = SummarizedExperiment::colData(object),
#'     "metadata" = S4Vectors::metadata(object),
#'     "reducedDims" = SingleCellExperiment::reducedDims(object)
#' )
#' sce <- do.call(what = makeSingleCellExperiment, args = args)
#' print(sce)
makeSingleCellExperiment <-
    function(...,
             reducedDims = S4Vectors::SimpleList()) {
        assert(isAny(reducedDims, c("SimpleList", "list", "NULL")))
        if (!is(reducedDims, "SimpleList")) {
            reducedDims <- SimpleList(reducedDims) # nocov
        }
        ## Don't enforce lower camel case here.
        if (hasLength(reducedDims)) {
            assert(hasValidNames(reducedDims)) # nocov
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
