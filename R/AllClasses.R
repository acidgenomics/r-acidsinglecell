#' Cell-cycle markers
#'
#' Data provenence information, including the organism and Ensembl release are
#' defined in `metadata()`.
#'
#' @export
#' @note Updated 2021-09-13.
#'
#' @return `CellTypeMarkers`
setClass(
    Class = "CellCycleMarkers",
    contains = "CompressedSplitDataFrameList"
)
setValidity(
    Class = "CellCycleMarkers",
    method = function(object) {
        validate(
            areSetEqual(
                x = colnames(object[[1L]]),
                y = c("geneId", "geneName", "phase")
            )
            ## > isSubset(
            ## >     x = c("date", "organism", "provider", "release"),
            ## >     y = names(metadata(object))
            ## > )
        )
    }
)



#' Cell-type markers
#'
#' Data provenence information, including the organism and Ensembl release are
#' defined in `metadata()`.
#'
#' @export
#' @note Updated 2021-09-13.
#'
#' @return `CellTypeMarkers`
setClass(
    Class = "CellTypeMarkers",
    contains = "CompressedSplitDataFrameList"
)
setValidity(
    Class = "CellTypeMarkers",
    method = function(object) {
        validate(
            areSetEqual(
                x = colnames(object[[1L]]),
                y = c("cellType", "geneId", "geneName")
            )
            ## > isSubset(
            ## >     x = c("date", "organism", "provider", "release"),
            ## >     y = names(metadata(object))
            ## > )
        )
    }
)



#' Known markers
#'
#' Class containing known markers detected.
#'
#' Results are grouped by `cellType` column and arranged by adjusted *P* value
#' (`padj`).
#'
#' @export
#' @note Updated 2021-09-13.
#'
#' @return `KnownMarkers`.
setClass(
    Class = "KnownMarkers",
    contains = "DataFrame"
)
setValidity(
    Class = "KnownMarkers",
    method = function(object) {
        validate(
            ## Consider requiring "avgLog2Fc" column here.
            isSubset(
                x = c(
                    "cellType",
                    "cluster",
                    "geneId",
                    "geneName",
                    "name",
                    "padj",
                    "pvalue"
                ),
                y = colnames(object)
            ),
            isSubset(
                x = c("alphaThreshold", "date", "packageVersion"),
                y = names(metadata(object))
            )
        )
    }
)
