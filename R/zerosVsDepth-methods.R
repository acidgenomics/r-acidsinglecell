#' @name zerosVsDepth
#' @author Michael Steinbaugh, Rory Kirchner
#' @inherit AcidGenerics::zerosVsDepth
#' @note Updated 2022-10-24.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @examples
#' data(SingleCellExperiment_splatter, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_splatter
#' df <- zerosVsDepth(object)
#' summary(df)
#' colnames(df)
NULL



## Updated 2019-07-22.
`zerosVsDepth,matrix` <- # nolint
    function(object) {
        present <- object > 0L
        DataFrame(
            "dropout" = (nrow(present) - colSums(present)) / nrow(present),
            "depth" = as.integer(colSums(object)),
            row.names = colnames(object)
        )
    }



## Using a logical matrix is faster and more memory efficient.
## Updated 2022-10-24.
`zerosVsDepth,sparseMatrix` <- # nolint
    function(object) {
        present <- as(object, "lMatrix")
        DataFrame(
            dropout = (nrow(present) - colSums(present)) / nrow(present),
            depth = as.integer(colSums(object)),
            row.names = colnames(object)
        )
    }



## Updated 2019-08-06.
`zerosVsDepth,SE` <- # nolint
    function(object, assay = 1L) {
        assert(isScalar(assay))
        counts <- assay(object, i = assay)
        data <- zerosVsDepth(counts)
        sampleData <- sampleData(object)
        assert(
            identical(rownames(data), rownames(sampleData)),
            areDisjointSets(colnames(data), colnames(sampleData))
        )
        cbind(data, sampleData)
    }



## Updated 2019-08-11.
`zerosVsDepth,SCE` <- # nolint
    function(object, assay = 1L) {
        assert(isScalar(assay))
        counts <- assay(object, i = assay)
        data <- zerosVsDepth(counts)
        data[["sampleId"]] <- cell2sample(object)
        sampleData <- sampleData(object)
        sampleData[["sampleId"]] <- as.factor(rownames(sampleData))
        assert(
            is(data, "DataFrame"),
            is(sampleData, "DataFrame")
        )
        out <- leftJoin(x = data, y = sampleData, by = "sampleId")
        assert(
            is(out, "DataFrame"),
            hasRownames(out)
        )
        out
    }



#' @rdname zerosVsDepth
#' @export
setMethod(
    f = "zerosVsDepth",
    signature = signature(object = "SingleCellExperiment"),
    definition = `zerosVsDepth,SCE`
)

#' @rdname zerosVsDepth
#' @export
setMethod(
    f = "zerosVsDepth",
    signature = signature(object = "SummarizedExperiment"),
    definition = `zerosVsDepth,SE`
)

#' @rdname zerosVsDepth
#' @export
setMethod(
    f = "zerosVsDepth",
    signature = signature(object = "matrix"),
    definition = `zerosVsDepth,matrix`
)

#' @rdname zerosVsDepth
#' @export
setMethod(
    f = "zerosVsDepth",
    signature = signature(object = "sparseMatrix"),
    definition = `zerosVsDepth,sparseMatrix`
)
