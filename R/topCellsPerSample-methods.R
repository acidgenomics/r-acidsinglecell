#' @name topCellsPerSample
#' @inherit AcidGenerics::topCellsPerSample
#' @note Updated 2021-02-02.
#'
#' @inheritParams AcidRoxygen::params
#' @param n `integer(1)`.
#'   Number of barcodes to return per sample.
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' x <- topCellsPerSample(object)
#' lapply(x, head)
NULL



## Updated 2021-02-02.
`topCellsPerSample,SCE` <-  # nolint
    function(object, n = 100L) {
        validObject(object)
        assert(isInt(n))
        cell2sample <- cell2sample(object)
        counts <- counts(object)
        if (is(counts, "Matrix")) {
            colSums <- Matrix::colSums
        }
        colSums <- colSums(counts)
        assert(identical(names(cell2sample), names(colSums)))
        data <- DataFrame(
            "cellId" = names(cell2sample),
            "sampleId" = cell2sample,
            "n" = colSums
        )
        split <- split(data, f = data[["sampleId"]])
        assert(is(split, "SplitDataFrameList"))
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
    signature = signature("SingleCellExperiment"),
    definition = `topCellsPerSample,SCE`
)
