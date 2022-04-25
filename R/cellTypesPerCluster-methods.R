#' @name cellTypesPerCluster
#' @inherit AcidGenerics::cellTypesPerCluster
#' @note Updated 2021-10-15.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param min,max `integer(1)`.
#' Minimum or maximum number of marker genes per cluster.
#'
#' @return `DataFrame`.
#'
#' @examples
#' data(KnownMarkers, package = "AcidTest")
#'
#' ## KnownMarkers ====
#' object <- KnownMarkers
#' x <- cellTypesPerCluster(object)
#' print(x)
NULL



## Updated 2021-10-15.
`cellTypesPerCluster,KnownMarkers` <- # nolint
    function(object,
             min = 1L,
             max = Inf) {
        validObject(object)
        assert(
            isSubset(
                x = c(
                    "avgLog2Fc",
                    "cellType",
                    "cluster",
                    "geneId",
                    "geneName",
                    "name",
                    "padj"
                ),
                y = colnames(object)
            ),
            allArePositive(c(min, max)),
            isInt(min),
            isInt(max)
        )
        x <- as(object, "DataFrame")
        ## Only positive markers are informative here.
        keep <- x[["avgLog2Fc"]] > 0L
        x <- x[keep, , drop = FALSE]
        x <- x[order(x[["padj"]]), , drop = FALSE]
        vars <- c("cluster", "cellType")
        f <- .group(x[, vars])
        split <- split(x = x, f = f)
        ## Summarize the data per split.
        ## Using `toString()` instead of `aggregate()` for markdown tables.
        ## Genes are arranged by adjusted P value.
        split <- SplitDataFrameList(lapply(
            X = split,
            FUN = function(x) {
                ## NOTE Consider using `CharacterList` here instead?
                DataFrame(
                    "cluster" = x[["cluster"]][[1L]],
                    "cellType" = x[["cellType"]][[1L]],
                    "name" = toString(x[["name"]]),
                    "geneId" = toString(x[["geneId"]]),
                    "geneName" = toString(x[["geneName"]]),
                    "n" = nrow(x)
                )
            }
        ))
        x <- unlist(split, recursive = FALSE, use.names = FALSE)
        ## Apply minimum and maximum gene cutoffs.
        if (
            isTRUE(is.numeric(min)) &&
                isTRUE(min > 1L)
        ) {
            keep <- x[["n"]] >= min
            x <- x[keep, , drop = FALSE]
        }
        if (
            isTRUE(is.numeric(max)) &&
                isTRUE(max > 1L)
        ) {
            keep <- x[["n"]] <= max
            x <- x[keep, , drop = FALSE]
        }
        assert(hasRows(x))
        x <- x[order(x[["cluster"]], -x[["n"]]), , drop = FALSE]
        x <- droplevels2(x)
        x
    }



#' @rdname cellTypesPerCluster
#' @export
setMethod(
    f = "cellTypesPerCluster",
    signature = signature(object = "KnownMarkers"),
    definition = `cellTypesPerCluster,KnownMarkers`
)
