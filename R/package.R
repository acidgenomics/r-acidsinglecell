#' AcidSingleCell
#'
#' Toolkit for single-cell RNA-seq analysis that extends the functionality
#' of SingleCellExperiment.
#'
#' @keywords internal
#'
#' @importClassesFrom SingleCellExperiment SingleCellExperiment
#' @importClassesFrom pipette Matrix
#'
#' @importMethodsFrom SingleCellExperiment coerce
#' @importMethodsFrom pipette coerce
#'
#' @importFrom AcidBase barcodePattern formalsList initDir methodFormals
#'   methodFunction packageName packageVersion printString realpath
#'   standardizeCall
#' @importFrom AcidCLI abort alert alertInfo alertSuccess alertWarning dl txt ul
#'   verbatim
#' @importFrom AcidExperiment assay assays calculateMetrics colData colData<-
#'   counts counts<- decode encode interestingGroups matchInterestingGroups
#'   makeSummarizedExperiment matchSampleColumn rowRanges sampleNames
#'   uniteInterestingGroups
#' @importFrom AcidGenerics DataFrame DataFrameList SimpleList coerce colSums
#'   do.call head metadata metadata<- na.omit rowSums split
#' @importFrom AcidPlyr leftJoin mutateAll
#' @importFrom SingleCellExperiment SingleCellExperiment reducedDimNames
#'   reducedDim
#' @importFrom goalie allAreMatchingRegex areDisjointSets assert bapply
#'   hasColnames hasLength hasMetrics hasNames hasNoDuplicates
#'   hasNonzeroRowsAndCols hasRownames hasRows hasValidNames isAny isCharacter
#'   isFlag isInLeftOpenRange isInRange isInt isIntegerish isNonNegative
#'   isPositive isScalar isString isSubset
#' @importFrom methods as is validObject
#' @importFrom pipette Matrix as_tibble assignAndSaveData tibble
#' @importFrom scales percent
#' @importFrom stringr str_match
#' @importFrom syntactic camelCase
"_PACKAGE"
