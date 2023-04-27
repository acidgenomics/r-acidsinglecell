#' @name cellCountsPerCluster
#' @inherit AcidGenerics::cellCountsPerCluster
#' @note Updated 2023-04-27.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @return `DFrame`.
#'
#' @examples
#' data(SingleCellExperiment_Seurat, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' object <- SingleCellExperiment_Seurat
#' x <- cellCountsPerCluster(object)
#' print(x)
NULL



## Updated 2022-05-05.
`cellCountsPerCluster,SCE` <- # nolint
    function(object) {
        validObject(object)
        assert(hasClusters(object))
        interestingGroups <- interestingGroups(object)
        x <- metrics(object)
        ## Contingency table.
        tbl <- table(x[["ident"]], x[["sampleId"]])
        ## Get the number of cells per ident.
        nPerIdent <- rowSums(tbl)
        nPerIdent <- DataFrame(
            "ident" = names(nPerIdent),
            "nPerIdent" = as.integer(nPerIdent)
        )
        ## Summarize to cluster/sample-level metadata.
        cols <- unique(c(
            "ident", "sampleId", "sampleName",
            interestingGroups, "interestingGroups"
        ))
        x <- unique(x[, cols, drop = FALSE])
        rownames(x) <- NULL
        x <- x[order(x[["ident"]], x[["sampleId"]]), , drop = FALSE]
        ## Melt the contingency table into long format.
        melt <- melt(tbl, colnames = c("ident", "sampleId", "n"))
        ## Join and calculate ratio.
        x <- leftJoin(x, melt, by = c("ident", "sampleId"))
        x <- leftJoin(x, nPerIdent, by = "ident")
        x[["ratio"]] <- x[["n"]] / x[["nPerIdent"]]
        x
    }



#' @rdname cellCountsPerCluster
#' @export
setMethod(
    f = "cellCountsPerCluster",
    signature = signature(object = "SingleCellExperiment"),
    definition = `cellCountsPerCluster,SCE`
)
