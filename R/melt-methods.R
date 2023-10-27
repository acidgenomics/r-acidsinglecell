#' @name melt
#' @inherit AcidPlyr::melt
#' @note Updated 2023-10-27.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' dim(object)
#' df <- melt(object)
#' nrow(df)
#' print(df)
NULL



## Updated 2023-10-27.
`melt,SCE` <- # nolint
    function(object) {
        assert(
            validObject(object),
            isScalar(assay)
        )
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
        colData <- metrics(object)
        keep <- which(bapply(colData, is.factor))
        colData <- colData[, keep, drop = FALSE]
        colData[[colnamesCol]] <- as.factor(rownames(colData))
        data <- leftJoin(data, colData, by = colnamesCol)
        data <- encode(data)
        data
    }

formals(`melt,SCE`) <- # nolint
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
