#' @name cell2sample
#' @inherit AcidGenerics::cell2sample
#' @note Updated 2022-03-02.
#'
#' @details
#' Sample identifiers must be defined in `sampleId` column in `colData()`.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return
#' - `"factor"`: Named `factor` containing the samples as the levels and cell
#'   identifiers as the names.
#' - `DataFrame`: Data frame containing `sampleId` column and cell identifiers
#'   as the row names.
#' - `"tbl_df"`: Tibble containing `cellId` and `sampleId` columns.
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' c2s <- cell2sample(object)
#' table(c2s)
NULL



## Updated 2021-02-02.
`cell2sample,SCE` <-  # nolint
    function(
        object,
        return = c("factor", "DataFrame", "tbl_df")
    ) {
        validObject(object)
        return <- match.arg(return)
        colData <- colData(object)
        sampleCol <- matchSampleColumn(colData)
        assert(isSubset(sampleCol, colnames(colData)))
        cells <- colnames(object)
        samples <- colData[[sampleCol]]
        if (!is.factor(samples)) {
            samples <- as.factor(samples)  # nocov
        }
        switch(
            EXPR = return,
            "DataFrame" = {
                out <- DataFrame(
                    "cellId" = cells,
                    "sampleId" = samples,
                    row.names = cells
                )
            },
            "factor" = {
                out <- samples
                names(out) <- cells
            },
            "tbl_df" = {
                out <- tibble(
                    "cellId" = cells,
                    "sampleId" = samples
                )
            }
        )
        out
    }



#' @rdname cell2sample
#' @export
setMethod(
    f = "cell2sample",
    signature = signature(object = "SingleCellExperiment"),
    definition = `cell2sample,SCE`
)
