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
#' @importFrom AcidCLI alert
#' @importFrom AcidExperiment counts counts<- interestingGroups
#'   matchSampleColumn
#' @importFrom AcidGenerics metadata metadata<-
#' @importFrom S4Vectors SimpleList
#' @importFrom SingleCellExperiment SingleCellExperiment reducedDimNames
#'   reducedDim
#' @importFrom SummarizedExperiment colData colData<- rowRanges
#' @importFrom goalie areDisjointSets assert hasColnames hasRownames hasRows
#'   isString isSubset
#' @importFrom methods validObject
#' @importFrom utils packageName packageVersion
NULL



#' @importFrom methods coerce
#' @importMethodsFrom SingleCellExperiment coerce
#' @importMethodsFrom SummarizedExperiment coerce
#' @exportMethod coerce
NULL
