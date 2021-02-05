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
#' @importMethodsFrom SingleCellExperiment coerce
#' @importMethodsFrom SummarizedExperiment coerce
#'
#' @importFrom AcidBase barcodePattern formalsList initDir methodFormals
#'   methodFunction printString realpath standardizeCall
#' @importFrom AcidCLI alert alertInfo alertSuccess alertWarning dl txt ul
#'   verbatim
#' @importFrom AcidExperiment calculateMetrics counts counts<- decode encode
#'   interestingGroups matchInterestingGroups makeSummarizedExperiment
#'   matchSampleColumn sampleNames uniteInterestingGroups
#' @importFrom AcidGenerics do.call head metadata metadata<- na.omit split
#' @importFrom AcidPlyr leftJoin mutateAll
#' @importFrom IRanges DataFrameList
#' @importFrom S4Vectors DataFrame SimpleList
#' @importFrom SingleCellExperiment SingleCellExperiment reducedDimNames
#'   reducedDim
#' @importFrom SummarizedExperiment assay assays colData colData<- rowRanges
#' @importFrom goalie allAreMatchingRegex areDisjointSets assert bapply
#'   hasColnames hasLength hasMetrics hasNames hasNoDuplicates
#'   hasNonzeroRowsAndCols hasRownames hasRows hasValidNames isAny isCharacter
#'   isFlag isInLeftOpenRange isInRange isInt isIntegerish isNonNegative
#'   isPositive isScalar isString isSubset
#' @importFrom methods as is validObject
#' @importFrom pipette as_tibble assignAndSaveData tibble
#' @importFrom scales percent
#' @importFrom stringr str_match
#' @importFrom syntactic camelCase
#' @importFrom utils packageName packageVersion
NULL
