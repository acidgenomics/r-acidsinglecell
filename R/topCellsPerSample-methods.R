#' @name topCellsPerSample
#' @inherit AcidGenerics::topCellsPerSample
#' @note Updated 2022-03-02.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param n `integer(1)`.
#' Number of barcodes to return per sample.
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' x <- topCellsPerSample(object)
#' lapply(x, head)
NULL



## Updated 2021-02-13.
`topCellsPerSample,SCE` <- # nolint
    function(object, n = 100L) {
        validObject(object)
        assert(isInt(n))
        c2s <- cellToSample(object)
        counts <- counts(object)
        colSums <- colSums(counts)
        assert(identical(names(c2s), names(colSums)))
        data <- DataFrame(
            "cellId" = names(c2s),
            "sampleId" = c2s,
            "n" = colSums
        )
        split <- split(data, f = data[["sampleId"]])
        assert(is(split, "SplitDFrameList"))
        lapply(
            X = split,
            FUN = function(x) {
                order <- order(x[["n"]], decreasing = TRUE)
                x <- x[order, , drop = FALSE]
                x <- head(x, n = n)
                x[["cellId"]]
            }
        )
    }



#' @rdname topCellsPerSample
#' @export
setMethod(
    f = "topCellsPerSample",
    signature = signature(object = "SingleCellExperiment"),
    definition = `topCellsPerSample,SCE`
)
