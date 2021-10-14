#' @name aggregateCellsToSamples
#' @inherit AcidGenerics::aggregateCellsToSamples
#' @note Updated 2021-09-13.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @details
#' Internally this function automatically obtains the cell-to-sample groupings
#' and then performs a sum aggregation with the `aggregate()` function.
#'
#' @examples
#' data(SingleCellExperiment_lanesplit, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' x <- SingleCellExperiment_lanesplit
#' x <- aggregateCellsToSamples(x)
#' print(x)
NULL



## Updated 2021-09-13.
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
        aggregate(
            x = rse,
            col = aggregateCol,
            fun = "sum",
            MARGIN = 2L
        )
    }



#' @rdname aggregateCellsToSamples
#' @export
setMethod(
    f = "aggregateCellsToSamples",
    signature = signature(x = "SingleCellExperiment"),
    definition = `aggregateCellsToSamples,SCE`
)
