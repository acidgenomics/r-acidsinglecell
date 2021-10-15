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
#' @importFrom AcidBase barcodePattern initDir methodFormals methodFunction
#'   model.matrix packageName packageVersion printString realpath relevel
#'   requireNamespaces standardizeCall
#' @importFrom AcidCLI abort alert alertInfo alertSuccess alertWarning dl h1 h2
#'   toInlineString txt ul verbatim
#' @importFrom AcidExperiment assay assayNames assays calculateMetrics colData
#'   colData<- counts counts<- decode encode estimateSizeFactors
#'   interestingGroups makeSummarizedExperiment matchInterestingGroups
#'   matchSampleColumn rowRanges sampleNames sizeFactors uniteInterestingGroups
#' @importFrom AcidGenerics DataFrame DataFrameList SimpleList
#'   SplitDataFrameList coerce colSums complete.cases do.call head metadata
#'   metadata<- na.omit rowSums split tail
#' @importFrom AcidGenomes makeGene2SymbolFromEnsembl
#' @importFrom AcidPlyr leftJoin mutateAll mutateIf
#' @importFrom SingleCellExperiment SingleCellExperiment reducedDimNames
#'   reducedDim
#' @importFrom goalie allAreMatchingRegex allArePositive areDisjointSets
#'   areSetEqual assert bapply hasColnames hasLength hasMetrics hasNames
#'   hasNoDuplicates hasNonzeroRowsAndCols hasRownames hasRows hasValidNames
#'   isAny isCharacter isFlag isInLeftOpenRange isInRange isInt isIntegerish
#'   isNonNegative isPositive isScalar isString isSubset validate
#' @importFrom methods as is new setClass setValidity validObject
#' @importFrom pipette Matrix as_tibble assignAndSaveData import tibble
#' @importFrom scales percent
#' @importFrom stringr str_match
#' @importFrom syntactic camelCase snakeCase
"_PACKAGE"
