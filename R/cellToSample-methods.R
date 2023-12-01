#' @name cellToSample
#' @inherit AcidGenerics::cellToSample
#' @note Updated 2023-09-26.
#'
#' @details
#' Sample identifiers must be defined in `sampleId` column in `colData()`.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return
#' - `"factor"`: Named `factor` containing the samples as the levels and cell
#' identifiers as the names.
#' - `"DFrame"`: S4 data frame containing `sampleId` column and cell identifiers
#' as the row names.
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' c2s <- cellToSample(object)
#' table(c2s)
NULL



## Updated 2023-09-26.
`cellToSample,SCE` <- # nolint
    function(object,
             return = c("factor", "DFrame")) {
        validObject(object)
        return <- match.arg(return)
        colData <- colData(object)
        sampleCol <- matchSampleColumn(colData)
        assert(isSubset(sampleCol, colnames(colData)))
        cells <- colnames(object)
        samples <- colData[[sampleCol]]
        if (!is.factor(samples)) {
            samples <- as.factor(samples)
        }
        switch(
            EXPR = return,
            "DFrame" = {
                out <- DataFrame(
                    "cellId" = cells,
                    "sampleId" = samples,
                    row.names = cells
                )
            },
            "factor" = {
                out <- samples
                names(out) <- cells
            }
        )
        out
    }



#' @rdname cellToSample
#' @export
setMethod(
    f = "cellToSample",
    signature = signature(object = "SingleCellExperiment"),
    definition = `cellToSample,SCE`
)
