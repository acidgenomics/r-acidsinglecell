#' AcidSingleCell
#'
#' Toolkit for single-cell RNA-seq analysis that extends the functionality
#' of SingleCellExperiment.
#'
#' @keywords internal
"_PACKAGE"



#' @importClassesFrom Matrix Matrix
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#'
#' @importFrom AcidBase formalsList methodFormals methodFunction
#' @importFrom AcidExperiment matchSampleColumn
#' @importFrom SingleCellExperiment SingleCellExperiment reducedDimNames
#'   reducedDim
#' @importFrom SummarizedExperiment colData
#' @importFrom goalie areDisjointSets assert isString
#' @importFrom methods validObject
#' @importFrom utils packageName packageVersion
NULL



#' @importFrom methods coerce
#' @importMethodsFrom SingleCellExperiment coerce
#' @importMethodsFrom SummarizedExperiment coerce
#' @exportMethod coerce
NULL
