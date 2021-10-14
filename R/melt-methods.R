#' @name melt
#' @inherit AcidPlyr::melt
#' @note Updated 2021-02-02.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment
#' dim(object)
#' x <- melt(object)
#' nrow(x)
#' print(x)
NULL



## Updated 2019-08-26.
`melt,SCE` <-  # nolint
    function(object) {
        validObject(object)
        assert(isScalar(assay))
        minMethod <- match.arg(minMethod)
        trans <- match.arg(trans)
        counts <- assay(object, i = assay)
        data <- melt(
            object = counts,
            min = min,
            minMethod = minMethod,
            trans = trans
        )
        colnamesCol <- colnames(data)[[2L]]
        colData <- metrics(object, return = "DataFrame")
        keep <- which(bapply(colData, is.factor))
        colData <- colData[, keep, drop = FALSE]
        colData[[colnamesCol]] <- rownames(colData)
        data <- leftJoin(data, colData, by = colnamesCol)
        data <- encode(data)
        data
    }

formals(`melt,SCE`) <-
    methodFormals(
        f = "melt",
        signature = "SummarizedExperiment",
        package = "AcidExperiment"
    )



#' @rdname melt
#' @export
setMethod(
    f = "melt",
    signature = signature(object = "SingleCellExperiment"),
    definition = `melt,SCE`
)
