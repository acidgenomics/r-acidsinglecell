#' @name aggregateCellsToSamples
#' @inherit AcidGenerics::aggregateCellsToSamples
#' @note Updated 2021-02-08.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @details
#' Internally `aggregateCellsToSamples()` automatically obtains the
#' cell-to-sample groupings and then performs a sum aggregation with the
#' `aggregateCols()` function.
#'
#' @examples
#' data(SingleCellExperiment_lanesplit, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' x <- SingleCellExperiment_lanesplit
#' x <- aggregateCellsToSamples(x)
#' print(x)
NULL



## Updated 2021-02-05.
`aggregateCellsToSamples,SCE` <-  # nolint
    function(x) {
        validObject(x)
        rse <- as(x, "RangedSummarizedExperiment")
        colData <- colData(rse)
        aggregateCol <- ".cell2sample"
        assert(areDisjointSets(aggregateCol, colnames(colData)))
        colData[[aggregateCol]] <- cell2sample(x)
        sampleCol <- matchSampleColumn(colData)
        if (isSubset(sampleCol, colnames(colData))) {
            colData[[sampleCol]] <- NULL
        }
        colData(rse) <- colData
        aggregateCols(x = rse, col = aggregateCol, fun = "sum")
    }



#' @rdname aggregateCellsToSamples
#' @export
setMethod(
    f = "aggregateCellsToSamples",
    signature = signature("SingleCellExperiment"),
    definition = `aggregateCellsToSamples,SCE`
)
