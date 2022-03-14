#' @name clusters
#' @inherit AcidGenerics::clusters
#' @note Updated 2021-10-14..
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @seealso
#' - `Seurat::Idents()`.
#' - `monocle3::clusters()`.
#'
#' @examples
#' data(SingleCellExperiment_Seurat, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_Seurat
#' x <- clusters(object)
#' head(x)
#' table(x)
NULL



## Updated 2019-08-06.
`clusters,SCE` <- # nolint
    function(object) {
        validObject(object)
        assert(isSubset("ident", colnames(colData(object))))
        ident <- colData(object)[["ident"]]
        assert(is.factor(ident))
        names(ident) <- colnames(object)
        ident
    }



#' @rdname clusters
#' @export
setMethod(
    f = "clusters",
    signature = signature(object = "SingleCellExperiment"),
    definition = `clusters,SCE`
)
