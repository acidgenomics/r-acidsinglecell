## NOTE Consider adding pseudobulk support here in a future update.



#' @name diffExpPerCluster
#' @inherit AcidGenerics::diffExpPerCluster
#' @note Updated 2021-09-14.
#'
#' @inheritParams AcidRoxygen::params
#' @inheritParams diffExp
#'
#' @param group `character(1)`.
#' Group of interest for differential expression per cluster.
#' Must be a `factor` column in `colData()`.
#'
#' @param ... Passthrough arguments to `diffExp()`.
#'
#' @note Cluster identity (`ident`) must be defined in `colData()` for this
#' function to work.
#'
#' @return `list` containing:
#' - `caller = "edgeR"`: `DGELRT`.
#' - `caller = "DESeq2"`: `DESeqResults`.
#'
#' @examples
#' data(SingleCellExperiment_Seurat, package = "AcidTest")
#'
#' ## SingleCellExperiment ====
#' if (goalie::isInstalled("edgeR")) {
#'     object <- SingleCellExperiment_Seurat
#'     group <- factor(c("group1", "group2"))
#'     SummarizedExperiment::colData(object)$group <- group
#'     suppressMessages({
#'         x <- diffExpPerCluster(
#'             object = object,
#'             group = "group",
#'             numerator = "group2",
#'             denominator = "group1",
#'             caller = "edgeR"
#'         )
#'     })
#'     class(x)
#'     lapply(x, class)
#' }
NULL



## Updated 2020-01-30.
`diffExpPerCluster,SCE` <- # nolint
    function(object,
             group,
             numerator,
             denominator,
             ...) {
        alert("Running differential expression per cluster.")
        object <- as(object, "SingleCellExperiment")
        ## group
        assert(
            isString(group),
            isSubset(group, colnames(colData(object)))
        )
        groupings <- colData(object)[[group]]
        assert(is.factor(groupings))
        ## numerator
        assert(
            isString(numerator),
            isSubset(numerator, levels(groupings))
        )
        ## denominator
        assert(
            isString(denominator),
            isSubset(denominator, levels(groupings)),
            areDisjointSets(numerator, denominator)
        )
        ## Get the cluster identities.
        ident <- clusters(object)
        assert(is.factor(ident))
        clusters <- levels(ident)
        assert(length(clusters) >= 2L)
        alertInfo(sprintf("%d clusters detected.", length(clusters)))
        ## Loop across each cluster and perform pairwise DE based on the single
        ## group of interest defined.
        ## Consider adding a skip step here for clusters with very few cells.
        list <- lapply(
            X = clusters,
            FUN = function(cluster) {
                alert(sprintf("Cluster %s", as.character(cluster)))
                ## Subset the cells by cluster.
                cells <- colnames(object)[which(ident == cluster)]
                assert(hasLength(cells))
                subset <- object[, cells]
                ## Ensure that both the numerator and denominator are defined.
                groupings <- droplevels(colData(subset)[[group]])
                numerator <- colnames(subset)[which(groupings == numerator)]
                denominator <- colnames(subset)[which(groupings == denominator)]
                diffExp(
                    object = object,
                    numerator = numerator,
                    denominator = denominator,
                    ...
                )
            }
        )
        names(list) <- clusters
        list
    }



#' @rdname diffExpPerCluster
#' @export
setMethod(
    f = "diffExpPerCluster",
    signature = signature(object = "SingleCellExperiment"),
    definition = `diffExpPerCluster,SCE`
)
